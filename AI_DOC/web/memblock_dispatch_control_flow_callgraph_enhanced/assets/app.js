(function () {
  "use strict";

  const $ = (selector) => document.querySelector(selector);

  const flowSelect = $("#flow-select");
  const depthSelect = $("#depth-select");
  const searchInput = $("#search-input");
  const focusButton = $("#focus-neighborhood");
  const resetButton = $("#reset-view");
  const searchCount = $("#search-count");
  const searchResults = $("#search-results");
  const flowSummary = $("#flow-summary");
  const flowTitle = $("#flow-title");
  const flowStats = $("#flow-stats");
  const flowGraph = $("#flow-graph");
  const mobilePreviewToggle = $("#mobile-preview-toggle");
  const detailNavigation = $("#detail-navigation");
  const functionDetail = $("#function-detail");
  const functionLogic = $("#function-logic");
  const callChain = $("#call-chain");

  let activeFlowId = "overview";
  let activeFunction = "tc_dispatch_real_smoke::main_phase";
  let focusMode = false;
  let mobilePreviewMode = false;
  const navigationStack = [];

  function fn(id, meta) {
    return Object.assign({
      id,
      kind: "task/function",
      file: "",
      input: "无",
      output: "无",
      purpose: "",
      role: "",
      sideEffects: [],
      calls: [],
      external: false,
      source: "",
      logicNotes: [],
    }, meta);
  }

  const functionCatalog = {
    "uvm_default_sequence_start": fn("uvm_default_sequence_start", {
      kind: "UVM boundary",
      file: "UVM phase/default_sequence",
      input: "sequencer main_phase default_sequence 配置",
      output: "并行启动 LSQENQ/LINTSISSUE/LSQCOMMIT/REDIRECT/L2TLB sequence",
      purpose: "表示 UVM 框架自动拉起 agent default sequence 的外部边界。",
      role: "不是源码中的业务函数，但它解释了为什么多个 sequence 与主控 sequence 并行运行。",
      external: true,
    }),
    "dut_ready_fire_boundary": fn("dut_ready_fire_boundary", {
      kind: "DUT boundary",
      file: "DUT interface ready/fire",
      input: "driver valid/payload 与 DUT ready/canAccept",
      output: "接口 fire 或等待/abort",
      purpose: "表示真实 DUT 接口握手边界。",
      role: "driver 只在 DUT 接受后才允许软件状态推进。",
      external: true,
    }),
    "tc_dispatch_real_smoke::main_phase": fn("tc_dispatch_real_smoke::main_phase", {
      file: "mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_smoke.sv",
      input: "uvm_phase",
      output: "启动 real smoke 主控 sequence",
      purpose: "真实 dispatch smoke testcase 的 main phase 入口。",
      role: "置起 dispatch_real_smoke_active，然后调用父类 main_phase 和 testcase 的 real smoke sequence。",
      sideEffects: ["memblock_sync_pkg::dispatch_real_smoke_active = 1"],
      calls: ["tc_base::main_phase", "run_real_smoke_sequence"],
    }),
    "run_real_smoke_sequence": fn("run_real_smoke_sequence", {
      file: "mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_smoke.sv",
      input: "无",
      output: "memblock_main_dispatch_auto_build_main_table_base_sequence.start(null)",
      purpose: "创建并启动真实 dispatch 主控 virtual sequence。",
      role: "把 testcase 层切到 sequence 层。",
      calls: ["memblock_main_dispatch_auto_build_main_table_base_sequence::body"],
    }),
    "tc_base::build_phase": fn("tc_base::build_phase", {
      file: "mem_ut/ver/ut/memblock/tc/src/tc_base.sv",
      input: "uvm_phase",
      output: "创建 env；绑定 agent default_sequence",
      purpose: "测试环境构建入口。",
      role: "配置真实 dispatch flow 需要的 LSQENQ、LINTSISSUE、LSQCOMMIT、REDIRECT、L2TLB 默认 sequence。",
      calls: ["memblock_env::build_phase", "uvm_default_sequence_start"],
    }),
    "tc_base::main_phase": fn("tc_base::main_phase", {
      file: "mem_ut/ver/ut/memblock/tc/src/tc_base.sv",
      input: "uvm_phase",
      output: "维持 testcase main phase objection",
      purpose: "testcase main phase 基础控制。",
      role: "为子类 real smoke 提供统一 phase 运行框架。",
    }),
    "memblock_env::build_phase": fn("memblock_env::build_phase", {
      file: "mem_ut/ver/ut/memblock/env/src/memblock_env.sv",
      input: "uvm_phase",
      output: "agent、FIFO、RM、SCB 创建完成",
      purpose: "构建 memblock UVM 环境组件。",
      role: "为后续 driver/monitor/analysis_port 建立对象实例。",
      calls: ["memblock_env::connect_phase"],
    }),
    "memblock_env::connect_phase": fn("memblock_env::connect_phase", {
      file: "mem_ut/ver/ut/memblock/env/src/memblock_env.sv",
      input: "uvm_phase",
      output: "monitor analysis_port 接入 FIFO/RM",
      purpose: "连接环境组件。",
      role: "让 monitor 采样事件能进入 scoreboard/RM 或 raw event 通道。",
    }),

    "memblock_main_dispatch_auto_build_main_table_base_sequence::body": fn("memblock_main_dispatch_auto_build_main_table_base_sequence::body", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_auto_build_main_table_base_sequence.sv",
      input: "无",
      output: "主表构建、服务循环、end check",
      purpose: "真实 dispatch flow 的主控 sequence 入口。",
      role: "不直接驱动 DUT issue/commit 接口，而是构建公共表并周期性服务 monitor、replay/redirect 和 route。",
      calls: ["memblock_dispatch_base_sequence::pre_body", "build_main_table", "service_real_dispatch_flow", "common_data_transaction::end_test_check"],
    }),
    "memblock_dispatch_base_sequence::pre_body": fn("memblock_dispatch_base_sequence::pre_body", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv",
      input: "无",
      output: "seq_csr_common 初始化；helper 句柄准备",
      purpose: "所有 dispatch sequence 的公共初始化。",
      role: "保证 plus 参数、common_data、scheduler、adapter、handler 等在 randomize/驱动前可用。",
      calls: ["seq_csr_common::init", "common_data_transaction::get", "lsq_ctrl_model::reset"],
    }),
    "seq_csr_common::init": fn("seq_csr_common::init", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv",
      input: "plus/config",
      output: "测试框架参数初始化完成",
      purpose: "统一解析公共测试框架参数。",
      role: "主表大小、pipe 数、send_pri、replay/redirect、TLB 权重等参数都要在 randomize 前完成。",
    }),
    "common_data_transaction::get": fn("common_data_transaction::get", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv",
      input: "无",
      output: "公共数据单例句柄",
      purpose: "获取主表、状态表、TLB 表、issue queue 和 raw event 之间共享的数据中心。",
      role: "所有 sequence/handler 通过同一个 common_data_transaction 协同状态。",
    }),
    "common_data_transaction::check_uid": fn("common_data_transaction::check_uid", { file: "common_data_transaction.sv", input: "uid, caller", output: "越界 fatal 或通过", purpose: "检查 uid 是否在主表范围内。", role: "公共表访问前的防御式校验，避免错误 uid 污染状态表。" }),
    "common_data_transaction::get_status": fn("common_data_transaction::get_status", { file: "common_data_transaction.sv", input: "uid", output: "status_transaction", purpose: "读取 uid 对应运行时状态。", role: "route、commit、redirect、success prefix 推进都依赖它获取当前状态真源。" }),
    "lsq_ctrl_model::reset": fn("lsq_ctrl_model::reset", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_ctrl_model.sv",
      input: "无",
      output: "本地 LQ/SQ 指针和 free count 回到初始状态",
      purpose: "初始化软件 LSQ 资源模型。",
      role: "入队前必须知道 LQ/SQ 是否有空间，commit/deq 后也依赖它释放资源。",
    }),

    "build_main_table": fn("build_main_table", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv",
      input: "plus 参数 / 手动主表",
      output: "main table、status table、main_table_ready",
      purpose: "生成或导入测试主控制表。",
      role: "主表是后续 admission、TLB、issue、commit、replay 共同引用的事实源。",
      calls: ["build_random_main_table", "import_manual_main_table", "init_status_for_main_table", "common_data_transaction::check_main_table_complete"],
    }),
    "build_random_main_table": fn("build_random_main_table", {
      file: "memblock_dispatch_base_sequence.sv",
      input: "main_trans_num_i",
      output: "随机 transaction 主表",
      purpose: "按 plus 参数随机生成 transaction。",
      role: "连续分配 uid 和 ROB key，并对每条 transaction 做最小合法化。",
      calls: ["common_data_transaction::reset_all_tables", "common_data_transaction::alloc_uid", "choose_rob_start_key", "choose_addr_ref_window", "randomize_main_transaction", "prune_recent_uid_q", "apply_addr_reuse_window", "common_data_transaction::set_main_transaction", "push_recent_uid", "rob_order_util::rob_advance", "init_status_for_main_table", "common_data_transaction::check_main_table_complete"],
    }),
    "import_manual_main_table": fn("import_manual_main_table", {
      file: "memblock_dispatch_base_sequence.sv",
      input: "manual_main_table_by_rob",
      output: "按手动配置导入主表",
      purpose: "支持 testcase 手动配置关联数组，不随机生成主表。",
      role: "用于复现定向场景，仍然会分配 uid、校验和初始化状态。",
      calls: ["common_data_transaction::reset_all_tables", "common_data_transaction::alloc_uid", "validate_main_table_entry", "common_data_transaction::set_main_transaction"],
    }),
    "randomize_main_transaction": fn("randomize_main_transaction", {
      file: "memblock_dispatch_base_sequence.sv",
      input: "main_control_transaction tr, uid, rob_key",
      output: "字段随机且被合法化的主表项",
      purpose: "完成单条主表 transaction 的随机化。",
      role: "选择 op class、fuOpType、地址、优先级、延迟、ROB/LQ/SQ 相关基础字段。",
      calls: ["select_op_class_by_weight", "apply_minimal_op_template", "apply_legal_addr_template", "randomize_send_pri_value", "randomize_delay_value", "validate_main_table_entry"],
    }),
    "select_op_class_by_weight": fn("select_op_class_by_weight", {
      file: "memblock_dispatch_base_sequence.sv",
      input: "plus 权重",
      output: "memblock_op_class_e",
      purpose: "按权重选择 load/store/amo/prefetch 等操作类别。",
      role: "决定后续最小合法模板的方向。",
    }),
    "apply_minimal_op_template": fn("apply_minimal_op_template", {
      file: "memblock_dispatch_base_sequence.sv",
      input: "main_control_transaction",
      output: "fuType/fuOpType/lsq_flow/numLsElem 修正",
      purpose: "把随机 transaction 修正成最小合法 LS 操作。",
      role: "避免 fuOpType 与 LSQ flow 完全无约束导致非法激励。",
      calls: ["random_load_fuoptype", "random_store_fuoptype", "random_prefetch_fuoptype", "random_amo_fuoptype"],
    }),
    "random_load_fuoptype": fn("random_load_fuoptype", { file: "memblock_dispatch_base_sequence.sv", input: "plus 权重", output: "load fuOpType", purpose: "随机选择 load 类 fuOpType。", role: "作为 load 模板的 opcode 来源。" }),
    "random_store_fuoptype": fn("random_store_fuoptype", { file: "memblock_dispatch_base_sequence.sv", input: "plus 权重", output: "store fuOpType", purpose: "随机选择 store 类 fuOpType。", role: "作为 STA/STD 模板的 opcode 来源。" }),
    "random_prefetch_fuoptype": fn("random_prefetch_fuoptype", { file: "memblock_dispatch_base_sequence.sv", input: "plus 权重", output: "prefetch fuOpType", purpose: "随机选择 prefetch 类 fuOpType。", role: "当前属于保守 LS flow 扩展入口。" }),
    "random_amo_fuoptype": fn("random_amo_fuoptype", { file: "memblock_dispatch_base_sequence.sv", input: "plus 权重", output: "AMO fuOpType", purpose: "随机选择 atomic 类 fuOpType。", role: "当前简化模型下作为 atomic flow 标记。" }),
    "apply_legal_addr_template": fn("apply_legal_addr_template", {
      file: "memblock_dispatch_base_sequence.sv",
      input: "main_control_transaction",
      output: "src/imm/vaddr/paddr 合法化",
      purpose: "生成可被 TLB/LSQ 使用的地址字段。",
      role: "给后续 TLB 表、L2TLB responder 和 RAW 违例注入提供地址基础。",
    }),
    "randomize_send_pri_value": fn("randomize_send_pri_value", { file: "memblock_dispatch_base_sequence.sv", input: "is_std", output: "send_pri/send_pri_std", purpose: "生成 issue 优先级。", role: "send_pri_mode_en 开启时按权重随机，关闭时返回 default。" }),
    "randomize_delay_value": fn("randomize_delay_value", { file: "memblock_dispatch_base_sequence.sv", input: "plus 权重", output: "ready delay", purpose: "生成发射延迟。", role: "让 issue queue item 不是全部立即 eligible。" }),
    "validate_main_table_entry": fn("validate_main_table_entry", { file: "memblock_dispatch_base_sequence.sv", input: "transaction, caller", output: "fatal 或通过", purpose: "检查主表项是否合法。", role: "防止非法主表流入 LSQ/TLB/issue 后才暴露问题。" }),
    "choose_rob_start_key": fn("choose_rob_start_key", { file: "memblock_dispatch_base_sequence.sv", input: "ROB start plus 参数", output: "uid0 ROB key", purpose: "选择随机主表 ROB 起始 value。", role: "初始 flag 固定为 0，后续仍由 rob_advance 连续推进。" }),
    "choose_addr_ref_window": fn("choose_addr_ref_window", { file: "memblock_dispatch_base_sequence.sv", input: "窗口 fixed/权重参数", output: "uid 距离窗口", purpose: "选择 recent queue 保留范围。", role: "约束地址复用只使用仍可能在 LSQ 内的近距离候选。" }),
    "prune_recent_uid_q": fn("prune_recent_uid_q", { file: "memblock_dispatch_base_sequence.sv", input: "recent uid queue, cur_uid, window", output: "删除过期候选", purpose: "淘汰 uid 距离超过窗口的参考项。", role: "避免旧后处理全表扫描选到距离太远的 transaction。" }),
    "apply_addr_reuse_window": fn("apply_addr_reuse_window", {
      file: "memblock_dispatch_base_sequence.sv",
      input: "当前 transaction、uid、recent load/store queue",
      output: "可能修正类型和地址字段",
      purpose: "主表生成期按 recent-window 提高 load/store 地址相关概率。",
      role: "每个 uid 只尝试一种复用场景，命中后复制窗口内参考 transaction 的 src_0/imm。",
      calls: ["select_addr_reuse_kind", "random_pick_recent_uid", "set_transaction_ls_kind", "fixup_after_addr_reuse", "validate_main_table_entry"],
    }),
    "select_addr_reuse_kind": fn("select_addr_reuse_kind", { file: "memblock_dispatch_base_sequence.sv", input: "四类地址复用权重", output: "memblock_addr_reuse_kind_e", purpose: "选择 LOAD/STORE_AFTER_LOAD/STORE 场景。", role: "把旧两类方向权重收敛为生成期四类 after 枚举。" }),
    "random_pick_recent_uid": fn("random_pick_recent_uid", { file: "memblock_dispatch_base_sequence.sv", input: "recent uid queue, delete_after_pick", output: "ref_uid", purpose: "从窗口候选随机选参考 uid。", role: "同类型复用可删除候选，跨类型复用保留候选。" }),
    "set_transaction_ls_kind": fn("set_transaction_ls_kind", { file: "memblock_dispatch_base_sequence.sv", input: "transaction, make_load", output: "op_class/fuType/fuOpType/lsq_flow 修正", purpose: "第一层类型修正。", role: "保证地址复用改类型后主表字段仍是合法 load/store 模板。" }),
    "fixup_after_addr_reuse": fn("fixup_after_addr_reuse", { file: "memblock_dispatch_base_sequence.sv", input: "transaction, ref_tr, copy_addr", output: "vaddr 更新并校验", purpose: "第二层复用后修正。", role: "负责复制 src_0/imm、重算 vaddr 并调用 validate_main_table_entry。" }),
    "push_recent_uid": fn("push_recent_uid", { file: "memblock_dispatch_base_sequence.sv", input: "最终 transaction, uid", output: "uid 入 recent load/store queue", purpose: "维护后续地址复用候选。", role: "入队依据最终类型，而不是初始随机类型。" }),
    "init_status_for_main_table": fn("init_status_for_main_table", {
      file: "memblock_dispatch_base_sequence.sv",
      input: "main table",
      output: "status_by_uid 初始化",
      purpose: "为每条主表项建立运行时状态表。",
      role: "后续 active、admitted、dispatched、writeback、commit、deq、success 都写在 status 表。",
      calls: ["common_data_transaction::init_status_for_uid"],
    }),
    "common_data_transaction::reset_all_tables": fn("common_data_transaction::reset_all_tables", { file: "common_data_transaction.sv", input: "main_trans_num_i", output: "所有表/队列清空", purpose: "测试开始前清理公共数据。", role: "保证一轮 testcase 不继承上一轮残留状态。" }),
    "common_data_transaction::alloc_uid": fn("common_data_transaction::alloc_uid", { file: "common_data_transaction.sv", input: "无", output: "新的 uid", purpose: "给主表项分配唯一 uid。", role: "uid 是主表、状态表、TLB 表和 issue queue 的统一索引。" }),
    "common_data_transaction::set_main_transaction": fn("common_data_transaction::set_main_transaction", { file: "common_data_transaction.sv", input: "uid, tr", output: "main_table_by_uid[uid]", purpose: "写入主表项。", role: "建立 uid 到 transaction 的事实源。" }),
    "common_data_transaction::init_status_for_uid": fn("common_data_transaction::init_status_for_uid", { file: "common_data_transaction.sv", input: "uid", output: "status_transaction", purpose: "创建 status 表项。", role: "后续所有 runtime 状态更新的落点。" }),
    "common_data_transaction::check_main_table_complete": fn("common_data_transaction::check_main_table_complete", { file: "common_data_transaction.sv", input: "无", output: "main_table_ready=1", purpose: "主表构建完成检查。", role: "agent default sequence 只有看到 ready 后才开始驱动。" }),
    "common_data_transaction::mark_uid_enqueued": fn("common_data_transaction::mark_uid_enqueued", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv",
      input: "uid",
      output: "dispatch_progress.max_enqueued_uid 更新",
      purpose: "记录当前已成功 admission 的最大连续 uid。",
      role: "这是 10 万笔场景的公共入队进度，不让 LSQ admission 每次从 0 扫描；同时要求 admission 必须按 uid 顺序连续推进。",
      sideEffects: ["首次入队必须是 uid0", "非首次入队必须等于 max_enqueued_uid + 1"],
      source: String.raw`function void mark_uid_enqueued(input memblock_uid_t uid);
    check_uid(uid, "mark_uid_enqueued");
    if (!dispatch_progress.max_enqueued_uid_valid) begin
        if (uid != 0) begin
            ` + "`" + String.raw`uvm_fatal("COMMON_DATA",
                       $sformatf("first LSQ admission must be uid0, got uid=%0d", uid))
        end
        dispatch_progress.max_enqueued_uid       = uid;
        dispatch_progress.max_enqueued_uid_valid = 1'b1;
        return;
    end
    if (uid != dispatch_progress.max_enqueued_uid + 1) begin
        ` + "`" + String.raw`uvm_fatal("COMMON_DATA",
                   $sformatf("LSQ admission must be sequential: uid=%0d expected=%0d max_enqueued_uid=%0d",
                             uid,
                             dispatch_progress.max_enqueued_uid + 1,
                             dispatch_progress.max_enqueued_uid))
    end
    dispatch_progress.max_enqueued_uid = uid;
endfunction:mark_uid_enqueued`,
      logicNotes: [
        "max_enqueued_uid 可以理解为“已经送进 LSQ 的最远连续边界”。后续新 admission 从这个边界后一条开始，不需要从 uid0 全表找。",
        "这里强制连续顺序，是为了 redirect 后回退边界时能从最老 flush uid 重新 admission，避免跳过被 flush 的旧 uid。",
      ],
    }),
    "common_data_transaction::rollback_max_enqueued_uid": fn("common_data_transaction::rollback_max_enqueued_uid", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv",
      input: "oldest_flushed_uid",
      output: "max_enqueued_uid 回退到 oldest_flushed_uid - 1",
      purpose: "redirect flush 后把 admission 边界退回到最老被 flush uid 之前。",
      role: "保证被 redirect 覆盖的 uid 后续会按原 uid 顺序重新 admission，而不是继续从旧最大 uid 后面发新请求。",
      calls: ["common_data_transaction::check_uid"],
    }),
    "common_data_transaction::advance_terminal_done_uid": fn("common_data_transaction::advance_terminal_done_uid", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv",
      input: "status_by_uid",
      output: "dispatch_progress.terminal_done_uid 前移",
      purpose: "维护已经连续 terminal_done 的完成前缀边界。",
      role: "route、redirect flush 扫描、完成判定可以从 terminal_done_uid 后开始，避免 10 万笔场景反复扫描已经完成的 uid。",
      calls: ["common_data_transaction::get_status"],
      source: String.raw`function void advance_terminal_done_uid();
    status_transaction status;

    while (dispatch_progress.terminal_done_uid < main_trans_num) begin
        status = get_status(dispatch_progress.terminal_done_uid);
        if (!status.terminal_done) begin
            break;
        end
        dispatch_progress.terminal_done_uid++;
    end
endfunction:advance_terminal_done_uid`,
      logicNotes: [
        "terminal_done_uid 表示从 uid0 开始连续完成到哪里。它不是“最大完成 uid”，而是“最老未进入终态 uid”的下界。",
        "如果 uid3 已终态但 uid2 未终态，terminal_done_uid 仍停在 uid2，保证 ROB 顺序和 redirect recovery 不会误跳过 uid2。",
      ],
    }),
    "common_data_transaction::get_active_scan_begin_uid": fn("common_data_transaction::get_active_scan_begin_uid", { file: "common_data_transaction.sv", input: "dispatch_progress.terminal_done_uid", output: "扫描起点 uid", purpose: "返回活跃窗口扫描起点。", role: "redirect flush 和 route 不再从 uid0 开始扫，而是从最老未连续 terminal_done 的 uid 开始。" }),
    "common_data_transaction::get_active_scan_end_uid": fn("common_data_transaction::get_active_scan_end_uid", { file: "common_data_transaction.sv", input: "dispatch_progress.max_enqueued_uid", output: "扫描终点 uid，不包含 end", purpose: "返回当前已 admission 活跃窗口终点。", role: "限制 redirect flush 扫描只覆盖已经进入 LSQ 的 uid，避免 10 万笔未入队主表被无意义扫描。" }),
    "common_data_transaction::get_next_new_admit_uid": fn("common_data_transaction::get_next_new_admit_uid", {
      file: "common_data_transaction.sv",
      input: "dispatch_progress.max_enqueued_uid/max_enqueued_uid_valid",
      output: "下一条可尝试 admission 的 uid",
      purpose: "从公共入队进度推导下一条新 admission uid。",
      role: "替代 sequence 本地 next_admit_uid；redirect 后只要 rollback_max_enqueued_uid 回退，该函数自然返回老 uid 重新入队。",
      source: String.raw`function memblock_uid_t get_next_new_admit_uid();
    if (!dispatch_progress.max_enqueued_uid_valid) begin
        return 0;
    end
    return dispatch_progress.max_enqueued_uid + 1;
endfunction:get_next_new_admit_uid`,
    }),
    "common_data_transaction::set_status_field": fn("common_data_transaction::set_status_field", {
      file: "common_data_transaction.sv",
      input: "uid, memblock_status_field_e field, value",
      output: "status_by_uid[uid] 指定字段更新",
      purpose: "统一写状态表字段。",
      role: "状态更新入口会对 active/terminal_done/enq 等特殊字段做额外处理，例如 enq 置位时推进 max_enqueued_uid，terminal_done 置位时推进 terminal_done_uid。",
      calls: ["common_data_transaction::mark_uid_enqueued", "common_data_transaction::advance_terminal_done_uid"],
    }),

    "memblock_lsqenq_dispatch_base_sequence::body": fn("memblock_lsqenq_dispatch_base_sequence::body", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv",
      input: "default sequence 启动",
      output: "持续驱动 LSQ admission",
      purpose: "把主表中的 LS transaction 送入 DUT enqLsq。",
      role: "默认开启真实 admission；入队成功后激活 uid、构建 TLB、路由 issue queue。",
      calls: ["seq_csr_common::init", "memblock_lsqenq_dispatch_base_sequence::configure_from_plus", "memblock_lsqenq_dispatch_base_sequence::ensure_helpers", "wait_for_main_table", "drive_lsqenq_loop"],
    }),
    "memblock_lsqenq_dispatch_base_sequence::configure_from_plus": fn("memblock_lsqenq_dispatch_base_sequence::configure_from_plus", { file: "memblock_lsqenq_dispatch_base_sequence.sv", input: "seq_csr_common getter", output: "enq 参数", purpose: "读取 admission 相关参数。", role: "读取 enable、idle_stop、ready_timeout、start_timeout；LSQENQ 不再读取专用 max_cycles。" }),
    "memblock_lsqenq_dispatch_base_sequence::ensure_helpers": fn("memblock_lsqenq_dispatch_base_sequence::ensure_helpers", {
      file: "memblock_lsqenq_dispatch_base_sequence.sv",
      input: "无",
      output: "data/lsq_ctrl/tlb_builder/issue_sched/monitor_adapter 句柄",
      purpose: "准备 LSQ admission 所需 helper。",
      role: "确保后续入队、TLB 构建、issue route 和 CSR drain 都有可用对象。",
      calls: ["common_data_transaction::get"],
    }),
    "wait_for_main_table": fn("wait_for_main_table", { file: "multiple dispatch sequences", input: "common_data.main_table_ready", output: "允许 sequence 继续", purpose: "等待主表可用。", role: "保证 agent 不会在主表未构建时驱动非法空 transaction。" }),
    "drive_lsqenq_loop": fn("drive_lsqenq_loop", {
      file: "memblock_lsqenq_dispatch_base_sequence.sv",
      input: "无",
      output: "多拍 admission",
      purpose: "LSQ admission 主循环。",
      role: "forever loop 每拍尝试发送可入队 uid，无进展时累计 idle；连续空闲达到 idle_stop 后退出。",
      calls: ["send_lsqenq_cycle"],
    }),
    "send_lsqenq_cycle": fn("send_lsqenq_cycle", {
      file: "memblock_lsqenq_dispatch_base_sequence.sv",
      input: "cycle_idx",
      output: "本拍 xaction 发给 driver；成功后软件状态推进",
      purpose: "一拍 LSQ admission 的完整逻辑。",
      role: "先处理不需要 LSQ 分配的 uid，再收集 LSQ 候选、填 slot、等待 DUT 接收、确认响应。",
      calls: ["memblock_lsqenq_dispatch_base_sequence::apply_pending_lsq_cancels", "admit_non_lsq_if_ready", "collect_lsq_candidates", "clear_lsqenq_xaction", "assign_lsqenq_slot", "lsqenq_agent_driver::send_pkt", "confirm_lsq_candidates"],
      source: String.raw`task memblock_lsqenq_dispatch_base_sequence::send_lsqenq_cycle(input int unsigned cycle_idx,
                                                          output bit has_progress);
    has_progress = 1'b0;
    apply_pending_lsq_cancels();
    if (admit_non_lsq_if_ready(has_progress)) begin
        return;
    end
    if (!collect_lsq_candidates(uids, trs, behaviors, lq_keys, sq_keys)) begin
        return;
    end
    clear_lsqenq_xaction(tr);
    foreach (uids[idx]) begin
        assign_lsqenq_slot(tr, idx, uids[idx], trs[idx], behaviors[idx], lq_keys[idx], sq_keys[idx]);
    end
    start_item(tr);
    finish_item(tr);
    confirm_lsq_candidates(tr, uids, trs, behaviors, has_progress);
endtask:send_lsqenq_cycle`,
      logicNotes: [
        "该 task 是 admission 的单拍调度入口。第一步先消费 redirect flush 留下的软件 LQ/SQ cancel，保证本地 LSQ 镜像先回退，再允许同 uid 重新入队。",
        "只有 driver 完成 start_item/finish_item 并返回后，confirm_lsq_candidates 才提交软件状态，避免 DUT 未接受时提前更新 active/enq。",
      ],
    }),
    "admit_non_lsq_if_ready": fn("admit_non_lsq_if_ready", { file: "memblock_lsqenq_dispatch_base_sequence.sv", input: "next uid", output: "非 LSQ uid 可能 active/route", purpose: "处理 need_alloc=0 的简化流。", role: "不需要 DUT enqLsq 接口时仍然要激活状态并进入后续 route。" }),
    "memblock_lsqenq_dispatch_base_sequence::apply_pending_lsq_cancels": fn("memblock_lsqenq_dispatch_base_sequence::apply_pending_lsq_cancels", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv",
      input: "common_data.pending_lq_cancel_count / pending_sq_cancel_count",
      output: "lsq_ctrl 本地 LQ/SQ 指针和 free count 回退；pending cancel count 清零",
      purpose: "处理 redirect flush 后需要回滚的软件 LSQ admission 镜像。",
      role: "redirect reissue 前必须让本地 LSQ 模型与 DUT flush 后的资源状态重新一致，否则后续重入队会拿到错误的 lqIdx/sqIdx preview。",
      sideEffects: ["调用 lsq_ctrl.cancel_lq/cancel_sq", "清零 pending_lq_cancel_count/pending_sq_cancel_count"],
      calls: ["lsq_ctrl_model::cancel_lq", "lsq_ctrl_model::cancel_sq"],
      source: String.raw`function void memblock_lsqenq_dispatch_base_sequence::apply_pending_lsq_cancels();
    ensure_helpers();
    if (data.pending_lq_cancel_count != 0) begin
        // redirect flush后回退软件LSQ admission镜像，确保重入队仍与DUT response key一致。
        lsq_ctrl.cancel_lq(data.pending_lq_cancel_count);
        data.pending_lq_cancel_count = 0;
    end
    if (data.pending_sq_cancel_count != 0) begin
        // redirect flush后回退软件SQ admission镜像，后续uid从公共高水位顺序重入队。
        lsq_ctrl.cancel_sq(data.pending_sq_cancel_count);
        data.pending_sq_cancel_count = 0;
    end
endfunction:apply_pending_lsq_cancels`,
      logicNotes: [
        "pending_lq_cancel_count/pending_sq_cancel_count 是 redirect flush 时记录的待回退资源数，不在 flush 函数里直接改 lsq_ctrl，是为了让 LSQ admission sequence 在自己的时钟服务点统一恢复。",
        "这不是 DUT deq，而是 testbench 本地预测模型的回滚；DUT 真正释放资源仍来自 RTL flush/deq 行为。",
      ],
    }),
    "lsq_ctrl_model::cancel_lq": fn("lsq_ctrl_model::cancel_lq", { file: "lsq_ctrl_model.sv", input: "count", output: "本地 LQ enq pointer/free count 回退", purpose: "取消 redirect flush 覆盖的 LQ admission 预测。", role: "让重入队 lqIdx 分配重新对齐 DUT。" }),
    "lsq_ctrl_model::cancel_sq": fn("lsq_ctrl_model::cancel_sq", { file: "lsq_ctrl_model.sv", input: "count", output: "本地 SQ enq pointer/free count 回退", purpose: "取消 redirect flush 覆盖的 SQ admission 预测。", role: "让重入队 sqIdx 分配重新对齐 DUT。" }),
    "collect_lsq_candidates": fn("collect_lsq_candidates", {
      file: "memblock_lsqenq_dispatch_base_sequence.sv",
      input: "data.get_next_new_admit_uid()、free count、DUT 可接收约束",
      output: "本拍候选 uid/tr/behavior/lq/sq key",
      purpose: "选择本拍能入队的连续 uid。",
      role: "以 common_data 公共入队进度推导下一条新 admission uid，同时考虑 get_enq_per_cycle() 固定/随机上限、本地 LSQ free count、flush 阻塞和队列元素数。",
      calls: ["admission_blocked_by_flush", "next_uid_needs_lsq_admission", "lsq_ctrl_model::can_allocate", "lsq_ctrl_model::preview_allocate"],
    }),
    "admission_blocked_by_flush": fn("admission_blocked_by_flush", { file: "memblock_lsqenq_dispatch_base_sequence.sv", input: "common_data flush 状态", output: "bit", purpose: "判断 admission 是否被全局 flush 阻塞。", role: "redirect/flush 期间不允许新 uid 入队。" }),
    "next_uid_needs_lsq_admission": fn("next_uid_needs_lsq_admission", { file: "memblock_lsqenq_dispatch_base_sequence.sv", input: "data.get_next_new_admit_uid()", output: "uid/main_tr/behavior", purpose: "判断下一个 uid 是否需要 LSQ admission。", role: "不再维护本地 next_admit_uid，而是从 common_data 的 max_enqueued_uid 推导下一条 uid；redirect 后 max_enqueued_uid 回退，老 uid 会重新被扫描。" }),
    "clear_lsqenq_xaction": fn("clear_lsqenq_xaction", { file: "memblock_lsqenq_dispatch_base_sequence.sv", input: "lsqenq xaction", output: "slot 清零", purpose: "构造 xaction 前清空所有 slot。", role: "防止前一拍字段残留到本拍 DUT payload。" }),
    "assign_lsqenq_slot": fn("assign_lsqenq_slot", { file: "memblock_lsqenq_dispatch_base_sequence.sv", input: "候选 uid/tr/behavior/key", output: "lsqenq xaction slot", purpose: "填 DUT enqLsq payload。", role: "把 ROB、fuType、uopIdx/LQ/SQ/numLsElem 等写入接口 transaction。", calls: ["set_need_alloc", "set_req_fields"] }),
    "set_need_alloc": fn("set_need_alloc", { file: "memblock_lsqenq_dispatch_base_sequence.sv", input: "slot, need_alloc", output: "xaction.needAlloc", purpose: "设置本 slot 是否需要 LQ/SQ 分配。", role: "DUT enqLsq admission 的资源请求编码。" }),
    "set_req_fields": fn("set_req_fields", { file: "memblock_lsqenq_dispatch_base_sequence.sv", input: "slot payload 字段", output: "xaction.req fields", purpose: "写入 enqLsq req 详细字段。", role: "把主表 ROB/fuType/uopIdx 和 LSQ key 转换为 DUT payload。" }),
    "lsqenq_agent_driver::send_pkt": fn("lsqenq_agent_driver::send_pkt", {
      file: "mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_driver.sv",
      input: "lsqenq_agent_agent_xaction",
      output: "DUT valid/payload 驱动；response 采样",
      purpose: "真实驱动 enqLsq 接口。",
      role: "等待 canAccept，并在 flush_epoch 跨越时 abort 旧 admission。",
      calls: ["lsqenq_agent_driver::wait_lsq_can_accept", "lsqenq_agent_driver::sample_lsqenq_resp", "dut_ready_fire_boundary"],
    }),
    "lsqenq_agent_driver::wait_lsq_can_accept": fn("lsqenq_agent_driver::wait_lsq_can_accept", { file: "lsqenq_agent_driver.sv", input: "xaction", output: "canAccept 或 abort", purpose: "等待 DUT 入队资源。", role: "DUT 是最终真源，软件模型不能只按本地 free count 入队。" }),
    "lsqenq_agent_driver::sample_lsqenq_resp": fn("lsqenq_agent_driver::sample_lsqenq_resp", { file: "lsqenq_agent_driver.sv", input: "DUT resp", output: "xaction resp key", purpose: "采样 DUT 返回的 LQ/SQ 分配信息。", role: "confirm 阶段用它校验软件预览和 DUT 实际一致。" }),
    "get_resp_keys": fn("get_resp_keys", { file: "memblock_lsqenq_dispatch_base_sequence.sv", input: "xaction resp slot", output: "lq_key/sq_key", purpose: "从 DUT response 取实际分配 key。", role: "确认软件 preview 与 DUT 实际分配一致。" }),
    "confirm_lsq_candidates": fn("confirm_lsq_candidates", {
      file: "memblock_lsqenq_dispatch_base_sequence.sv",
      input: "xaction resp 与候选列表",
      output: "LSQ 模型提交、uid active、TLB/issue 后处理",
      purpose: "确认 admission 成功并推进软件状态。",
      role: "只有 DUT 接受后才允许更新 active map、TLB 表和 issue queue。",
      calls: ["get_resp_keys", "lsq_ctrl_model::commit_allocate_with_resp", "complete_admission"],
    }),
    "lsq_ctrl_model::can_allocate": fn("lsq_ctrl_model::can_allocate", { file: "lsq_ctrl_model.sv", input: "behavior/numLsElem", output: "bit", purpose: "预测本地 LQ/SQ 是否有空间。", role: "用于候选选择，但最终仍要以 DUT resp/deq 为准。" }),
    "lsq_ctrl_model::preview_allocate": fn("lsq_ctrl_model::preview_allocate", { file: "lsq_ctrl_model.sv", input: "behavior", output: "预期 lq/sq key", purpose: "预览本拍将分配的 key。", role: "填入 enq payload 并用于 confirm 对齐 DUT 返回。" }),
    "lsq_ctrl_model::commit_allocate_with_resp": fn("lsq_ctrl_model::commit_allocate_with_resp", { file: "lsq_ctrl_model.sv", input: "DUT resp key", output: "本地 LSQ 指针推进", purpose: "用 DUT 接收结果提交软件 LSQ 分配。", role: "避免 admission 失败时提前消耗本地 LQ/SQ 资源。" }),
    "complete_admission": fn("complete_admission", {
      file: "memblock_lsqenq_dispatch_base_sequence.sv",
      input: "uid",
      output: "active uid、TLB ready 状态、issue queue",
      purpose: "LSQ 入队后的统一后处理。",
      role: "真实源码中 active/enq 已在 lsq_ctrl commit 阶段完成；这里继续 drain CSR、标记 TLB_MAPPED，并 route 到 issue queue。实际 TLB entry 等 L2TLB request 到来时按 key 查/建。",
      calls: ["memblock_lsqenq_dispatch_base_sequence::drain_csr_runtime_events", "common_data_transaction::set_status_field", "issue_queue_scheduler::route_uid"],
    }),
    "common_data_transaction::activate_uid_by_behavior": fn("common_data_transaction::activate_uid_by_behavior", { file: "common_data_transaction.sv", input: "uid, behavior", output: "active ROB/LQ/SQ map", purpose: "根据操作行为激活 uid。", role: "建立 ROB/LQ/SQ 到 uid 的反查映射，用于 monitor/commit/deq 定位。" }),
    "memblock_lsqenq_dispatch_base_sequence::drain_csr_runtime_events": fn("memblock_lsqenq_dispatch_base_sequence::drain_csr_runtime_events", { file: "memblock_lsqenq_dispatch_base_sequence.sv", input: "latest_raw_csr", output: "runtime CSR 镜像更新", purpose: "LSQ admission 后立即同步 CSR runtime snapshot。", role: "保证随后 TLB 表构建使用最新 ASID/VMID/s2xlate。" }),

    "memblock_l2tlb_base_sequence::body": fn("memblock_l2tlb_base_sequence::body", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv",
      input: "default sequence 启动",
      output: "响应 DTLB->L2TLB request",
      purpose: "代替 L2TLB 对上游 DTLB 的 responder 功能。",
      role: "采 request 中的 vpn/s2xlate，结合 runtime CSR 查 common_data TLB 表并回填 response。",
      calls: ["seq_csr_common::init", "memblock_l2tlb_base_sequence::ensure_context", "memblock_l2tlb_base_sequence::configure_from_plus", "wait_for_main_table", "drive_l2tlb_loop"],
    }),
    "memblock_l2tlb_base_sequence::configure_from_plus": fn("memblock_l2tlb_base_sequence::configure_from_plus", { file: "memblock_l2tlb_base_sequence.sv", input: "plus getter", output: "latency/idle stop cycle", purpose: "读取 L2TLB responder 参数。", role: "控制 response latency 和连续 idle 退出阈值。" }),
    "memblock_l2tlb_base_sequence::ensure_context": fn("memblock_l2tlb_base_sequence::ensure_context", { file: "memblock_l2tlb_base_sequence.sv", input: "uvm_config_db vif / common_data", output: "data、monitor_adapter、l2tlb_vif 可用", purpose: "准备 L2TLB responder 所需上下文。", role: "保证 sequence 连接的是 DTLB->L2TLB 上游接口，而不是 L2TLB->L2Cache/PTW 下游接口。", calls: ["common_data_transaction::get"] }),
    "drive_l2tlb_loop": fn("drive_l2tlb_loop", { file: "memblock_l2tlb_base_sequence.sv", input: "无", output: "持续处理 request", purpose: "L2TLB responder 主循环。", role: "每拍采样 request，按 latency 查表并驱动 response；有 progress 清 idle_count，连续 idle 超过 idle_stop_cycle 后退出。", calls: ["send_l2tlb_cycle"] }),
    "send_l2tlb_cycle": fn("send_l2tlb_cycle", {
      file: "memblock_l2tlb_base_sequence.sv",
      input: "send_count",
      output: "response xaction 或 idle",
      purpose: "处理一拍 L2TLB request/response。",
      role: "request valid 时采 vpn/s2xlate，查表命中则回填 TLB 表项，并向上层返回 has_progress 以清零 idle_count。",
      calls: ["memblock_l2tlb_base_sequence::request_valid", "sample_request_fields", "memblock_l2tlb_base_sequence::create_l2tlb_xaction", "memblock_l2tlb_base_sequence::send_l2tlb_item", "memblock_l2tlb_base_sequence::choose_latency", "common_data_transaction::get_or_create_tlb_entry_by_req", "fill_dtlb_resp_from_entry", "common_data_transaction::update_uid_tlb_records_by_entry"],
      source: String.raw`task memblock_l2tlb_base_sequence::send_l2tlb_cycle(input int unsigned send_count,
                                                    output bit has_progress);
    has_progress = 1'b0;
    if (request_valid()) begin
        sample_request_fields(vpn, s2xlate);
        ready_tr = create_l2tlb_xaction($sformatf("l2tlb_ready_tr_send_%0d", send_count));
        ready_tr.io_ptw_req_0_ready = 1'b1;
        send_l2tlb_item(ready_tr);
        latency = choose_latency();
        resp_tr = create_l2tlb_xaction($sformatf("l2tlb_resp_tr_send_%0d", send_count));
        resp_tr.pre_pkt_gap = latency;
        drain_csr_runtime_events();
        if (data.get_or_create_tlb_entry_by_req(vpn, s2xlate, key, entry, created)) begin
            fill_dtlb_resp_from_entry(entry, resp_tr);
            record_update_count = data.update_uid_tlb_records_by_entry(key, entry);
        end else begin
            ` + "`" + String.raw`uvm_fatal(get_type_name(),
                       $sformatf("L2TLB entry miss for vpn=0x%0h s2xlate=%0d", vpn, s2xlate))
        end
        send_l2tlb_item(resp_tr);
        has_progress = 1'b1;
    end
endtask:send_l2tlb_cycle`,
      logicNotes: [
        "该 sequence 代替的是 L2TLB 对 DTLB 的上游响应，所以它先对 request ready，再按 pre_pkt_gap 延迟 response。",
        "缺项策略已经取消：get_or_create_tlb_entry_by_req 返回失败时直接 uvm_fatal，不再填默认缺项 response。",
      ],
    }),
    "sample_request_fields": fn("sample_request_fields", { file: "memblock_l2tlb_base_sequence.sv", input: "L2TLB interface", output: "vpn, s2xlate", purpose: "采 DTLB->L2TLB 请求关键字段。", role: "注意该 agent 代替 L2TLB，不是 L2TLB 到 L2Cache/PTW 下游模型。" }),
    "memblock_l2tlb_base_sequence::request_valid": fn("memblock_l2tlb_base_sequence::request_valid", { file: "memblock_l2tlb_base_sequence.sv", input: "l2tlb_vif.rst_n/reset_backend_done/io_ptw_req_0_valid", output: "bit", purpose: "判断当前拍是否有可接受的 DTLB->L2TLB request。", role: "过滤 reset 和后端未 ready 时的无效采样。" }),
    "memblock_l2tlb_base_sequence::create_l2tlb_xaction": fn("memblock_l2tlb_base_sequence::create_l2tlb_xaction", { file: "memblock_l2tlb_base_sequence.sv", input: "name", output: "已清零的 L2tlb_agent_agent_xaction", purpose: "创建 responder driver transaction。", role: "ready 包和 response 包都通过它创建，随后由 clear_l2tlb_xaction 消除字段残留。" , calls: ["memblock_l2tlb_base_sequence::clear_l2tlb_xaction"] }),
    "memblock_l2tlb_base_sequence::clear_l2tlb_xaction": fn("memblock_l2tlb_base_sequence::clear_l2tlb_xaction", { file: "memblock_l2tlb_base_sequence.sv", input: "L2tlb_agent_agent_xaction", output: "所有 request/response 字段清零", purpose: "清空 L2TLB xaction 默认值。", role: "防止上一拍 response 字段残留到下一拍 ready/resp 包。" }),
    "memblock_l2tlb_base_sequence::choose_latency": fn("memblock_l2tlb_base_sequence::choose_latency", { file: "memblock_l2tlb_base_sequence.sv", input: "min_latency/max_latency", output: "pre_pkt_gap latency", purpose: "选择 L2TLB response 延迟。", role: "不再通过发送空 wait transaction 延迟，而是直接写 resp_tr.pre_pkt_gap。" }),
    "memblock_l2tlb_base_sequence::send_l2tlb_item": fn("memblock_l2tlb_base_sequence::send_l2tlb_item", { file: "memblock_l2tlb_base_sequence.sv", input: "L2tlb_agent_agent_xaction", output: "start_item/finish_item 发给 driver", purpose: "发送 L2TLB ready/response transaction。", role: "把 responder sequence 生成的 xaction 交给 L2TLB agent driver。" }),
    "common_data_transaction::get_or_create_tlb_entry_by_req": fn("common_data_transaction::get_or_create_tlb_entry_by_req", { file: "common_data_transaction.sv", input: "vpn, s2xlate, runtime CSR", output: "key, entry, created", purpose: "按 by-key TLB cache 查/建 entry。", role: "key 包含 vpn/asid/vmid/s2xlate；miss 自动建 entry，失败则 L2TLB sequence fatal。", calls: ["common_data_transaction::build_tlb_entry_for_key"] }),
    "common_data_transaction::build_tlb_entry_for_key": fn("common_data_transaction::build_tlb_entry_for_key", { file: "common_data_transaction.sv", input: "memblock_tlb_lookup_key_t", output: "memblock_tlb_entry", purpose: "按 TLB key 构造 entry。", role: "集中调用 TLB entry randomize/fixup 逻辑，保证 response 权限、level、A/D 位合法化策略一致。" }),
    "fill_dtlb_resp_from_entry": fn("fill_dtlb_resp_from_entry", { file: "memblock_l2tlb_base_sequence.sv", input: "memblock_tlb_entry", output: "L2TLB response payload", purpose: "回填 PTE/PPN/权限/fault。", role: "把 by-key TLB entry 转换成 driver xaction。" }),
    "common_data_transaction::update_uid_tlb_records_by_entry": fn("common_data_transaction::update_uid_tlb_records_by_entry", { file: "common_data_transaction.sv", input: "key, entry", output: "匹配 uid record pte_valid=1", purpose: "回填 uid 追踪记录。", role: "PTW wait replay 和 debug 通过 uid record 判断 PTE 是否已回填。" }),

    "service_real_dispatch_flow": fn("service_real_dispatch_flow", {
      file: "memblock_main_dispatch_auto_build_main_table_base_sequence.sv",
      input: "主表已 ready",
      output: "所有 transaction success 或 timeout",
      purpose: "真实 dispatch flow 主服务循环。",
      role: "每个软件周期先处理 monitor raw event 和复杂事件，再 route issue queue，最后检查完成。",
      calls: ["service_monitor_once", "route_all_issue_queues", "all_transactions_terminal_done"],
    }),
    "service_monitor_once": fn("service_monitor_once", {
      file: "memblock_main_dispatch_auto_build_main_table_base_sequence.sv / memblock_dispatch_base_sequence.sv",
      input: "raw queues",
      output: "状态表/事件队列推进",
      purpose: "单轮 monitor service。",
      role: "统一 drain CSR runtime、sfence/hfence、writeback、ctrl、redirect/replay 相关 raw event。",
      calls: ["memblock_sync_pkg::tick_dispatch_service_cycle", "collect_runtime_context_events", "collect_monitor_event_batch", "exception_redirect_replay_task"],
    }),
    "memblock_sync_pkg::tick_dispatch_service_cycle": fn("memblock_sync_pkg::tick_dispatch_service_cycle", { file: "memblock_sync_pkg.sv", input: "无", output: "dispatch_service_cycle++", purpose: "推进 TB 软件服务周期。", role: "给 replay wait、flushSb timeout、redirect drive done 等逻辑提供统一时间基准。" }),
    "io_mem_to_ooo_int_wb_monitor::mon_data": fn("io_mem_to_ooo_int_wb_monitor::mon_data", {
      file: "mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_monitor.sv",
      input: "DUT int writeback ports",
      output: "raw_int_wb_q 增加采样项",
      purpose: "采集 LOAD/LSU writeback 侧真实输出。",
      role: "monitor 只记录事实，不直接改 status；后续由 adapter/handler 统一解释。",
      calls: ["memblock_sync_pkg::push_raw_int_wb"],
    }),
    "io_mem_to_ooo_iq_feedback_monitor::mon_data": fn("io_mem_to_ooo_iq_feedback_monitor::mon_data", {
      file: "mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_iq_feedback_agent_agent/src/io_mem_to_ooo_iq_feedback_agent_agent_monitor.sv",
      input: "DUT IQ feedback ports",
      output: "raw_iq_feedback_q 增加采样项",
      purpose: "采集 STA/STD feedback、pass/replay 信息。",
      role: "store 地址/数据侧 feedback 从这里进入统一 writeback event。",
      calls: ["memblock_sync_pkg::push_raw_iq_feedback"],
    }),
    "io_mem_to_ooo_ctrl_monitor::mon_data": fn("io_mem_to_ooo_ctrl_monitor::mon_data", {
      file: "mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_monitor.sv",
      input: "DUT ctrl ports",
      output: "raw_ctrl_q 增加采样项",
      purpose: "采集 lqDeq/sqDeq/memoryViolation/sbIsEmpty 等控制事件。",
      role: "commit/deq 和 redirect recovery 的 DUT 侧事实来源。",
      calls: ["memblock_sync_pkg::push_raw_ctrl"],
    }),
    "csr_ctrl_agent_agent_monitor::mon_data": fn("csr_ctrl_agent_agent_monitor::mon_data", {
      file: "CSR runtime monitor / memblock_sync_pkg.sv",
      input: "runtime CSR snapshot",
      output: "latest_raw_csr 被覆盖，latest_raw_csr_seq 递增",
      purpose: "采集实时 ASID/VMID/satp/vsatp/hgatp/s2xlate 上下文。",
      role: "TLB/L2TLB 查表必须使用 runtime CSR，不使用初始配置值。",
      calls: ["memblock_sync_pkg::push_raw_csr"],
    }),
    "fence_agent_agent_monitor::mon_data": fn("fence_agent_agent_monitor::mon_data", {
      file: "fence_agent_agent_monitor.sv",
      input: "io_ooo_to_mem_sfence_*",
      output: "raw_sfence_q 增加采样项",
      purpose: "采集 sfence/hfence 离散 TLB invalidation 事件。",
      role: "fence monitor 只记录事实；统一 service loop 在 CSR runtime 同步后 FIFO 消费。",
      calls: ["memblock_sync_pkg::push_raw_sfence"],
    }),
    "memblock_sync_pkg::push_raw_int_wb": fn("memblock_sync_pkg::push_raw_int_wb", { file: "memblock_sync_pkg.sv", input: "dispatch_raw_int_wb_t", output: "raw_int_wb_q.push_back", purpose: "缓存 int writeback raw event。", role: "把 monitor 采样与主控 service loop 解耦。" }),
    "memblock_sync_pkg::push_raw_iq_feedback": fn("memblock_sync_pkg::push_raw_iq_feedback", { file: "memblock_sync_pkg.sv", input: "dispatch_raw_iq_feedback_t", output: "raw_iq_feedback_q.push_back", purpose: "缓存 IQ feedback raw event。", role: "STA/STD pass/replay 之后由 adapter 消费。" }),
    "memblock_sync_pkg::push_raw_ctrl": fn("memblock_sync_pkg::push_raw_ctrl", { file: "memblock_sync_pkg.sv", input: "dispatch_raw_ctrl_t", output: "raw_ctrl_q.push_back", purpose: "缓存 ctrl raw event。", role: "LQ/SQ deq、memoryViolation、sbIsEmpty 通过该队列进入主控处理。" }),
    "memblock_sync_pkg::push_raw_csr": fn("memblock_sync_pkg::push_raw_csr", { file: "memblock_sync_pkg.sv", input: "dispatch_raw_csr_t", output: "latest_raw_csr/latest_raw_csr_seq", purpose: "缓存最新 CSR runtime snapshot。", role: "主控同步后更新 runtime CSR 镜像。" }),
    "memblock_sync_pkg::push_raw_sfence": fn("memblock_sync_pkg::push_raw_sfence", { file: "memblock_sync_pkg.sv", input: "dispatch_raw_sfence_t", output: "raw_sfence_q.push_back", purpose: "缓存 sfence/hfence raw event。", role: "sfence/hfence 是离散失效命令，必须 FIFO 消费，不能像 CSR snapshot 一样覆盖成 latest。" }),
    "collect_csr_runtime_events": fn("collect_csr_runtime_events", { file: "memblock_dispatch_base_sequence.sv", input: "latest_raw_csr", output: "runtime CSR 镜像更新", purpose: "同步 CSR runtime monitor snapshot。", role: "TLB lookup 和权限模式必须使用实时 CSR，而不是初始 csr_common。", calls: ["dispatch_monitor_event_adapter::drain_csr_events"] }),
    "collect_runtime_context_events": fn("collect_runtime_context_events", { file: "memblock_dispatch_base_sequence.sv", input: "latest_raw_csr/raw_sfence_q", output: "runtime CSR 镜像更新和 live TLB entry 失效", purpose: "统一 service loop 的 runtime context 入口。", role: "显式先同步 CSR runtime，再 FIFO 消费 sfence/hfence，避免 CSR-only 路径隐式清 fence queue。", calls: ["dispatch_monitor_event_adapter::drain_csr_events", "dispatch_monitor_event_adapter::drain_sfence_events"] }),
    "dispatch_monitor_event_adapter::drain_csr_events": fn("dispatch_monitor_event_adapter::drain_csr_events", { file: "dispatch_monitor_event_adapter.sv", input: "latest_raw_csr", output: "common_data CSR runtime state", purpose: "按 seq 把 latest CSR snapshot 写入 runtime 镜像。", role: "保证 L2TLB 查表使用最新 ASID/VMID/s2xlate 上下文。" }),
    "dispatch_monitor_event_adapter::drain_sfence_events": fn("dispatch_monitor_event_adapter::drain_sfence_events", { file: "dispatch_monitor_event_adapter.sv", input: "raw_sfence_q", output: "命中的 live TLB entry 被删除", purpose: "FIFO 消费 sfence/hfence raw event。", role: "把 fence monitor 采样交给 common_data_transaction::apply_raw_sfence() 执行 entry 级失效。" }),
    "collect_monitor_event_batch": fn("collect_monitor_event_batch", { file: "memblock_dispatch_base_sequence.sv", input: "raw_int_wb_q/raw_iq_feedback_q/raw_ctrl_q", output: "batch handler 消费同轮 semantic events", purpose: "真实 DUT flow 的统一 monitor batch 入口。", role: "同一 service cycle 内先收集 writeback/IQ feedback/memoryViolation，再由 batch handler 做 redirect-first 仲裁，避免 writeback 早于同拍 redirect 落状态。", calls: ["dispatch_monitor_event_adapter::collect_writeback_events_batch", "dispatch_monitor_event_adapter::collect_ctrl_redirect_events_batch", "dispatch_monitor_batch_handler::process_monitor_event_batch"] }),
    "dispatch_monitor_event_adapter::collect_writeback_events_batch": fn("dispatch_monitor_event_adapter::collect_writeback_events_batch", { file: "dispatch_monitor_event_adapter.sv", input: "raw_int_wb_q/raw_iq_feedback_q", output: "events[$] 追加 wb_event", purpose: "转换 writeback/feedback raw event，不直接写状态。", role: "只把 int_wb 和 IQ feedback 转成 memblock_wb_event_t 并放入本轮 batch。", calls: ["convert_raw_int_wb", "convert_raw_iq_feedback"] }),
    "dispatch_monitor_event_adapter::collect_ctrl_redirect_events_batch": fn("dispatch_monitor_event_adapter::collect_ctrl_redirect_events_batch", { file: "dispatch_monitor_event_adapter.sv", input: "raw_ctrl_q", output: "deq 即时更新；memoryViolation event 追加到 batch", purpose: "处理 ctrl raw event。", role: "LQ/SQ deq 直接交 commit handler；memoryViolation 转 redirect event 进入同轮 batch。", calls: ["dispatch_monitor_event_adapter::apply_raw_ctrl_deq", "convert_raw_memory_violation"] }),
    "dispatch_monitor_batch_handler::process_monitor_event_batch": fn("dispatch_monitor_batch_handler::process_monitor_event_batch", { file: "dispatch_monitor_batch_handler.sv", input: "events[$]", output: "状态更新或 exception_event_q", purpose: "batch 级 redirect-first 仲裁。", role: "先 normalize，再选择 oldest redirect；被 redirect 覆盖的 event 丢弃，未覆盖的非 redirect event 才交给 writeback_status_handler。", calls: ["common_data_transaction::normalize_feedback_event", "writeback_status_handler::handle_real_writeback_event", "writeback_status_handler::handle_issue_feedback_event", "common_data_transaction::push_feedback_event"] }),
    "convert_raw_int_wb": fn("convert_raw_int_wb", { file: "dispatch_monitor_event_adapter.sv", input: "dispatch_raw_int_wb_t", output: "memblock_wb_event_t", purpose: "转换 int writeback raw。", role: "用 ROB/LQ/SQ key 和 exception_vec 构造 load writeback/fault/redirect event。", calls: ["event_has_active_uid"] }),
    "convert_raw_iq_feedback": fn("convert_raw_iq_feedback", { file: "dispatch_monitor_event_adapter.sv", input: "dispatch_raw_iq_feedback_t", output: "memblock_wb_event_t", purpose: "转换 STA/STD IQ feedback。", role: "hit/pass 或 backend replay 都从这里进入统一 writeback event。" }),
    "event_has_active_uid": fn("event_has_active_uid", { file: "dispatch_monitor_event_adapter.sv", input: "wb_event key", output: "bit", purpose: "判断 raw event 是否能反查到 active uid。", role: "过滤被 flush 后残留的旧 monitor 事件。" }),
    "writeback_status_handler::event_is_redirect": fn("writeback_status_handler::event_is_redirect", { file: "writeback_status_handler.sv", input: "memblock_wb_event_t", output: "bit", purpose: "判断事件是否为 redirect。", role: "同时校验 redirect_valid 与 redirect.valid 必须一致，避免同一事件两套 valid 语义冲突。" }),
    "writeback_status_handler::handle_event": fn("writeback_status_handler::handle_event", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq_help/writeback_status_handler.sv",
      input: "memblock_wb_event_t",
      output: "bit consumed",
      purpose: "接收 adapter 转换后的事件，并按类型写状态或推复杂事件。",
      role: "只接受 batch handler 已 normalize 且放行的非 redirect event；redirect 仲裁和 active redirect 过滤已经在 dispatch_monitor_batch_handler 完成。",
      calls: ["writeback_status_handler::event_is_redirect", "writeback_status_handler::handle_real_writeback_event", "writeback_status_handler::handle_issue_feedback_event", "common_data_transaction::push_feedback_event"],
      source: [
        "function bit handle_event(input memblock_wb_event_t wb_event);",
        "    ensure_data();",
        "    if (!wb_event.valid) return 1'b0;",
        "    if (!wb_event.has_uid || !wb_event.has_rob || !wb_event.has_replay_seq) begin",
        "        `uvm_warning(\"WB_STATUS\", \"drop unnormalized feedback wb_event; monitor events must pass dispatch_monitor_batch_handler\")",
        "        return 1'b0;",
        "    end",
        "    if (event_is_redirect(wb_event)) begin",
        "        `uvm_warning(\"WB_STATUS\", \"drop redirect event: monitor redirect must enter dispatch_monitor_batch_handler\")",
        "        return 1'b0;",
        "    end",
        "    case (wb_event.source)",
        "        MEMBLOCK_WB_EVENT_SOURCE_LOAD_WB,",
        "        MEMBLOCK_WB_EVENT_SOURCE_STORE_WB: return handle_real_writeback_event(wb_event);",
        "        MEMBLOCK_WB_EVENT_SOURCE_STA_FEEDBACK,",
        "        MEMBLOCK_WB_EVENT_SOURCE_STD_FEEDBACK: return handle_issue_feedback_event(wb_event);",
        "        default: if (event_is_replay(wb_event)) begin",
        "            data.push_feedback_event(wb_event);",
        "            return 1'b1;",
        "        end",
        "    endcase",
        "    return 1'b0;",
        "endfunction:handle_event",
      ].join("\n"),
      logicNotes: [
        "monitor raw event 必须先经过 dispatch_monitor_batch_handler；未 normalized 的 event 会被拒绝。",
        "redirect event 不允许从 writeback handler 入口处理，防止绕过同批 redirect-first 仲裁。",
        "normal pass/fault/IQ feedback 由专门分支更新状态，replay 类复杂事件继续进入 common_data.feedback_event_q。",
      ],
    }),
    "common_data_transaction::normalize_feedback_event": fn("common_data_transaction::normalize_feedback_event", { file: "common_data_transaction.sv", input: "wb_event", output: "补齐 uid/target/epoch", purpose: "规范化反馈事件。", role: "将 ROB/LQ/SQ key 反查 uid，并统一 replay/redirect/fault 判断。" }),
    "common_data_transaction::mark_target_normal_pass": fn("common_data_transaction::mark_target_normal_pass", { file: "common_data_transaction.sv", input: "uid,target,issue_epoch,replay_seq", output: "target pass/writeback 状态", purpose: "标记目标通路正常完成。", role: "会校验 issue_epoch/replay_seq，防止旧反馈污染新发射。", calls: ["conditional_set_target_status_field", "required_targets_done"] }),
    "common_data_transaction::mark_target_fault": fn("common_data_transaction::mark_target_fault", { file: "common_data_transaction.sv", input: "uid,target,issue_epoch,replay_seq,exception_vec,cycle", output: "target fault/exception 状态", purpose: "标记目标通路异常完成。", role: "和 normal pass 一样会做版本匹配，匹配后写 fault、exception_pending 和异常向量，随后交 exception handler 做后续策略。" }),
    "conditional_set_target_status_field": fn("conditional_set_target_status_field", { file: "common_data_transaction.sv", input: "uid,target,field,epoch", output: "状态位可能更新", purpose: "带版本检查地设置 target 状态。", role: "统一处理旧 issue/replay 反馈过滤。" }),
    "required_targets_done": fn("required_targets_done", { file: "common_data_transaction.sv", input: "uid status", output: "bit", purpose: "判断该 uid 所需 target 是否都完成。", role: "决定 uid 是否可以进入 commit/success 后续条件。" }),
    "common_data_transaction::push_feedback_event": fn("common_data_transaction::push_feedback_event", { file: "common_data_transaction.sv", input: "complex wb_event", output: "feedback_event_q.push_back", purpose: "保存 replay/redirect/fault 复杂事件。", role: "让 exception_redirect_replay_handler 统一按优先级处理复杂恢复。" }),
    "dispatch_monitor_event_adapter::apply_raw_ctrl_deq": fn("dispatch_monitor_event_adapter::apply_raw_ctrl_deq", { file: "dispatch_monitor_event_adapter.sv", input: "raw ctrl", output: "commit handler deq 调用", purpose: "把 monitor ctrl deq 转给 LSQ commit handler。", role: "当前 ctrl deq 的唯一 adapter 入口。", calls: ["lsq_commit_handler::apply_raw_ctrl_deq"] }),
    "convert_raw_memory_violation": fn("convert_raw_memory_violation", { file: "dispatch_monitor_event_adapter.sv", input: "raw ctrl memoryViolation", output: "redirect wb_event", purpose: "把 memory violation 转成统一 redirect 事件。", role: "后续由 exception handler 冻结发射并驱动 redirect。" }),

    "route_all_issue_queues": fn("route_all_issue_queues", { file: "memblock_dispatch_base_sequence.sv", input: "无", output: "所有 ready uid 尝试入队", purpose: "主控循环中的 issue route 入口。", role: "把 admission/TLB/replay 后已 ready 的 uid 放入 LOAD/STA/STD 队列。", calls: ["issue_queue_scheduler::route_all_ready_uids"] }),
    "issue_queue_scheduler::route_all_ready_uids": fn("issue_queue_scheduler::route_all_ready_uids", { file: "issue_queue_scheduler.sv / base seq helper", input: "status/main/TLB 表", output: "issue queues 更新", purpose: "扫描所有 uid 并路由 ready 项。", role: "过滤 inactive、flush、未 TLB ready、已完成、replay mask 不匹配的 uid。", calls: ["issue_queue_scheduler::route_uid"] }),
    "issue_queue_scheduler::route_uid": fn("issue_queue_scheduler::route_uid", { file: "issue_queue_scheduler.sv / base seq helper", input: "uid", output: "目标队列可能 push item", purpose: "路由单个 uid。", role: "根据 op behavior 决定 LOAD、STA、STD 哪些 target 需要入队。", calls: ["lsq_ctrl_model::derive_op_behavior", "issue_queue_scheduler::route_target"] }),
    "lsq_ctrl_model::derive_op_behavior": fn("lsq_ctrl_model::derive_op_behavior", { file: "lsq_ctrl_model.sv / memblock_dispatch_base_sequence.sv", input: "main transaction", output: "memblock_op_behavior_t", purpose: "推导 load/store/atomic/cbo 的 LSQ 行为。", role: "决定是否需要 LQ/SQ、需要哪些 issue target、是否多 uop。" }),
    "issue_queue_scheduler::route_target": fn("issue_queue_scheduler::route_target", { file: "issue_queue_scheduler.sv / common_data_transaction.sv", input: "uid,target,behavior", output: "issue queue item", purpose: "向 LOAD/STA/STD 队列插入 item。", role: "防重复入队，并在 replay_pending 时只允许目标 replay target 入队。", calls: ["common_data_transaction::push_issue_queue_item"] }),
    "common_data_transaction::push_issue_queue_item": fn("common_data_transaction::push_issue_queue_item", { file: "common_data_transaction.sv", input: "memblock_issue_q_item_t", output: "目标 issue queue 增加 item", purpose: "保存待发射轻量项。", role: "队列项保存 uid/target/send_pri/replay_seq 等，真正 transaction 从主表按 uid 取。" }),

    "memblock_issue_dispatch_base_sequence::body": fn("memblock_issue_dispatch_base_sequence::body", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq/memblock_issue_dispatch_base_sequence.sv",
      input: "default sequence 启动",
      output: "持续发射 LOAD/STA/STD",
      purpose: "真实驱动 intIssue/load/STA/STD 发射接口。",
      role: "每拍 route、select、assign、drive，再只对 DUT 接收的 item 更新发射状态。",
      calls: ["seq_csr_common::init", "wait_for_main_table", "drive_dispatch_issue_loop"],
    }),
    "drive_dispatch_issue_loop": fn("drive_dispatch_issue_loop", { file: "memblock_issue_dispatch_base_sequence.sv", input: "无", output: "多拍 issue fire", purpose: "issue 发射主循环。", role: "按 plus 最大 service steps 不断调用 send_issue_cycle，并推进 issue queue delay。", calls: ["issue_queue_scheduler::route_all_ready_uids", "send_issue_cycle", "issue_queue_scheduler::advance_issue_queue_delays"] }),
    "issue_queue_scheduler::advance_issue_queue_delays": fn("issue_queue_scheduler::advance_issue_queue_delays", { file: "issue_queue_scheduler.sv / memblock_issue_dispatch_base_sequence.sv", input: "issue queues", output: "ready_cycle 递减", purpose: "推进队列项的延迟计数。", role: "让带 delay 的 transaction 到达可发射时刻。" }),
    "send_issue_cycle": fn("send_issue_cycle", { file: "memblock_issue_dispatch_base_sequence.sv", input: "cycle_idx", output: "本拍 issue xaction", purpose: "一拍 issue 发射。", role: "先 route ready uid，再选择候选、填 xaction、驱动、标记 fire。", calls: ["select_issue_candidates", "assign_issue_items", "lintsissue_agent_driver::send_pkt", "mark_fired_items"] }),
    "select_issue_candidates": fn("select_issue_candidates", { file: "memblock_dispatch_base_sequence.sv", input: "LOAD/STA/STD 队列", output: "本拍候选 item", purpose: "按 send_pri 模式、global 采样和 pipe 采样选择发射项。", role: "send_pri_mode_en 控制是否比较 priority，sample_global_send_pri_en 控制本拍是否跨队列过滤全局最大 priority。", calls: ["issue_queue_scheduler::select_issue_candidates"] }),
    "issue_queue_scheduler::select_issue_candidates": fn("issue_queue_scheduler::select_issue_candidates", { file: "issue_queue_scheduler.sv / base seq helper", input: "issue queues, pipe limit/random, send_pri mode/global weight", output: "selected load/sta/std items", purpose: "调度仲裁入口。", role: "统一处理优先级、ROB 年龄、pipe 采样、global filter 和 ready_cycle。", calls: ["select_target_candidates"] }),
    "select_target_candidates": fn("select_target_candidates", { file: "issue_queue_scheduler.sv / base seq helper", input: "target,max_count,compare_pri,use_global_pri,global_pri", output: "selected items", purpose: "在单 target 队列内挑选候选。", role: "不超过本拍采样 pipe 数，compare_pri 为 1 时按 send_pri/ROB 年龄筛选，use_global_pri 为 1 时先过滤非全局最大 priority。" }),
    "assign_issue_items": fn("assign_issue_items", { file: "memblock_issue_dispatch_base_sequence.sv", input: "selected items", output: "lintsissue xaction", purpose: "把候选 item 映射到 pipe payload。", role: "LOAD/STA/STD 分别填到可用发射端口。", calls: ["issue_field_assigner::assign_issue_item_fields"] }),
    "issue_field_assigner::assign_issue_item_fields": fn("issue_field_assigner::assign_issue_item_fields", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq_help/issue_field_assigner.sv",
      input: "lintsissue_agent_agent_xaction tr, memblock_issue_q_item_t item, pipe_idx",
      output: "完整 issue payload",
      purpose: "发射字段赋值总入口。",
      role: "真实源码中字段赋值逻辑在 issue_field_assigner.sv；base sequence 只持有/调用 helper。该函数先写主表字段，再补第二类依赖字段和第三类后端 meta 字段。",
      calls: ["assign_main_issue_fields", "assign_issue_dep_fields", "assign_backend_meta_fields"],
      source: String.raw`function void assign_issue_item_fields(input lintsissue_agent_agent_xaction tr,
                                       input memblock_issue_q_item_t item,
                                       input int unsigned pipe_idx);
    assign_main_issue_fields(tr, item, pipe_idx);
    assign_issue_dep_fields(tr, item, pipe_idx);
    assign_backend_meta_fields(tr, item, pipe_idx);
endfunction:assign_issue_item_fields`,
      logicNotes: [
        "这三个子函数对应用户前面定义的字段分类：主字段影响流水线控制，dep 字段影响次级行为，backend meta 字段主要随流水线到后端或写回侧使用。",
        "网页统一使用 issue_field_assigner:: 前缀，避免误以为字段赋值主实现仍在 memblock_dispatch_base_sequence.sv。",
      ],
    }),
    "assign_main_issue_fields": fn("assign_main_issue_fields", { file: "issue_field_assigner.sv", input: "main transaction", output: "fuType/fuOpType/robIdx/src/imm 等", purpose: "填直接影响 DUT 发射的主字段。", role: "这些字段决定 DUT 进入哪条 LS pipeline 以及执行什么操作。" }),
    "assign_issue_dep_fields": fn("assign_issue_dep_fields", { file: "issue_field_assigner.sv", input: "main/status", output: "loadWait/storeSet/isFirstIssue 等", purpose: "填影响次级行为的依赖字段。", role: "用于 MDP/等待/首次发射等行为建模。" }),
    "assign_backend_meta_fields": fn("assign_backend_meta_fields", { file: "issue_field_assigner.sv", input: "main transaction", output: "pc/isRVC/ftq/pdest/rfWen/fpWen 等", purpose: "填随流水线到后端的 meta 字段。", role: "通常不主导 MemBlock 控制流，但写回/后端追踪会使用。" }),
    "lintsissue_agent_driver::send_pkt": fn("lintsissue_agent_driver::send_pkt", { file: "mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_driver.sv", input: "lintsissue xaction", output: "valid/payload 驱动和 fired_mask", purpose: "真实驱动 issue 接口。", role: "只有 fire 的 pipe 才能更新发射状态；flush_epoch 变化时未 fire 端口 abort。", calls: ["dut_ready_fire_boundary"] }),
    "mark_fired_items": fn("mark_fired_items", { file: "memblock_issue_dispatch_base_sequence.sv", input: "fired_items, fired_mask", output: "status/queue 更新", purpose: "标记 DUT 已接受的发射项。", role: "防止未 ready 或 redirect abort 的 item 被错误删除；成功后可能构造 STD accept-pass 事件。", calls: ["issue_queue_scheduler::mark_issue_fire", "issue_queue_scheduler::mark_issue_fire_already_accepted", "submit_issue_accept_pass"] }),
    "issue_queue_scheduler::mark_issue_fire": fn("issue_queue_scheduler::mark_issue_fire", { file: "issue_queue_scheduler.sv / common_data_transaction.sv", input: "issue item", output: "dispatch 状态和 issue_epoch", purpose: "正常发射成功后的状态更新入口。", role: "删除队列项，保存发射快照，replay item fire 后清 replay target。", calls: ["common_data_transaction::mark_issue_snapshot", "common_data_transaction::delete_issue_queue_entry", "common_data_transaction::clear_replay_target_after_fire"] }),
    "issue_queue_scheduler::mark_issue_fire_already_accepted": fn("issue_queue_scheduler::mark_issue_fire_already_accepted", { file: "issue_queue_scheduler.sv / memblock_issue_dispatch_base_sequence.sv", input: "issue item", output: "已被 DUT 接收项的状态补记", purpose: "redirect/flush 已开始但 driver 已确认 fire 时的补偿标记。", role: "避免已被 DUT 接收的端口因为同拍 recovery 被遗漏。" }),
    "submit_issue_accept_pass": fn("submit_issue_accept_pass", { file: "memblock_issue_dispatch_base_sequence.sv", input: "issue item", output: "可能提交 STD_FEEDBACK pass event", purpose: "在未开启真实 STD writeback pass 时，给普通 store STD 构造 accept-pass。", role: "这是软件闭环开关控制的简化路径，正常真实 DUT feedback 打开时不生效。", calls: ["item_needs_issue_accept_pass", "make_issue_accept_pass_event", "writeback_status_handler::handle_event"] }),
    "item_needs_issue_accept_pass": fn("item_needs_issue_accept_pass", { file: "memblock_issue_dispatch_base_sequence.sv", input: "issue item", output: "bit", purpose: "判断该 STD item 是否需要软件构造 pass。", role: "仅 MEMBLOCK_STD_REAL_WB_PASS_EN=0 且普通 store STD 时返回真。" }),
    "make_issue_accept_pass_event": fn("make_issue_accept_pass_event", { file: "memblock_issue_dispatch_base_sequence.sv", input: "issue item", output: "memblock_wb_event_t", purpose: "构造 STD_FEEDBACK pass event。", role: "填 uid/ROB/SQ/issue_epoch/replay_seq，交给 writeback handler 走统一状态更新。" }),
    "common_data_transaction::mark_issue_snapshot": fn("common_data_transaction::mark_issue_snapshot", { file: "common_data_transaction.sv", input: "uid,target,pipe,epoch", output: "status issue snapshot", purpose: "记录发射版本。", role: "后续 writeback/pass/replay 用 issue_epoch 判断反馈是否属于当前发射。" }),
    "common_data_transaction::delete_issue_queue_entry": fn("common_data_transaction::delete_issue_queue_entry", { file: "common_data_transaction.sv", input: "target, uid, replay_seq", output: "队列项删除", purpose: "发射成功后删除 issue queue 项。", role: "追溯依赖主表/status，不需要发射队列保留历史项。" }),
    "common_data_transaction::clear_replay_target_after_fire": fn("common_data_transaction::clear_replay_target_after_fire", { file: "common_data_transaction.sv", input: "uid,target,replay_seq", output: "replay target mask 清除", purpose: "replay 重发成功后清目标位。", role: "所有目标清完后 replay_pending 可以解除。" }),

    "memblock_lsqcommit_dispatch_base_sequence::body": fn("memblock_lsqcommit_dispatch_base_sequence::body", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqcommit_dispatch_base_sequence.sv",
      input: "default sequence 启动",
      output: "驱动 pendingPtr/flushSb",
      purpose: "按 ROB 顺序驱动 LSQ commit 端口。",
      role: "提交不是 monitor 采样，而是 TB 主动按源码 commit 语义推 pendingPtr。",
      calls: ["seq_csr_common::init", "wait_for_main_table", "drive_lsqcommit_loop"],
    }),
    "drive_lsqcommit_loop": fn("drive_lsqcommit_loop", { file: "memblock_lsqcommit_dispatch_base_sequence.sv", input: "无", output: "多拍 commit drive", purpose: "commit sequence 主循环。", role: "每拍尝试 flushSb 或 pendingPtr commit。", calls: ["send_lsqcommit_cycle"] }),
    "send_lsqcommit_cycle": fn("send_lsqcommit_cycle", {
      file: "memblock_lsqcommit_dispatch_base_sequence.sv",
      input: "cycle_idx",
      output: "pendingPtr 或 flushSb xaction",
      purpose: "一拍 commit/flushSb 驱动。",
      role: "优先处理 flushSb，再构造 ROB commit batch。",
      calls: ["request_scheduled_flushsb_if_due", "drive_flushsb_if_needed", "lsq_commit_handler::build_lsqcommit_xaction", "lsqcommit_agent_driver::send_pkt", "lsq_commit_handler::mark_rob_commit_batch"],
    }),
    "request_scheduled_flushsb_if_due": fn("request_scheduled_flushsb_if_due", { file: "common_data_transaction.sv", input: "cycle_idx", output: "flushsb request 可能置位", purpose: "按计划周期触发 flushSb。", role: "用于测试 store buffer flush 行为。" }),
    "drive_flushsb_if_needed": fn("drive_flushsb_if_needed", { file: "memblock_lsqcommit_dispatch_base_sequence.sv", input: "cycle_idx", output: "flushSb xaction", purpose: "在需要时驱动 flushSb。", role: "驱动后等待 sbIsEmpty monitor 回来，期间阻塞相关完成判据。" }),
    "lsq_commit_handler::build_lsqcommit_xaction": fn("lsq_commit_handler::build_lsqcommit_xaction", { file: "lsq_commit_handler.sv", input: "当前 commit cursor/status", output: "commit_uids 与 pendingPtr xaction", purpose: "构造一拍 ROB commit。", role: "按 ROB 顺序选择连续可 commit uid，并让 pendingPtr 指向 batch 末尾。", calls: ["lsq_commit_handler::advance_commit_cursor_past_done", "lsq_commit_handler::select_rob_commit_batch"] }),
    "lsq_commit_handler::select_rob_commit_batch": fn("lsq_commit_handler::select_rob_commit_batch", { file: "lsq_commit_handler.sv", input: "commit cursor", output: "uids[$]", purpose: "选择 ROB 顺序 commit batch。", role: "只选择 required targets 已完成且未 flushed 的 active uid。", calls: ["lsq_commit_handler::uid_is_commit_candidate"] }),
    "lsq_commit_handler::uid_is_commit_candidate": fn("lsq_commit_handler::uid_is_commit_candidate", { file: "lsq_commit_handler.sv", input: "uid", output: "bit", purpose: "判断 uid 是否满足 commit 候选条件。", role: "select_rob_commit_batch 用它过滤还未完成 target、已 flush 或状态不合法的 uid。" }),
    "lsq_commit_handler::advance_commit_cursor_past_done": fn("lsq_commit_handler::advance_commit_cursor_past_done", { file: "lsq_commit_handler.sv", input: "commit cursor/status", output: "commit cursor 前移", purpose: "跳过已完成或不可再提交的 uid。", role: "保持 pendingPtr 选择与 ROB 顺序一致。" }),
    "lsqcommit_agent_driver::send_pkt": fn("lsqcommit_agent_driver::send_pkt", { file: "mem_ut/ver/ut/memblock/agent/lsqcommit_agent_agent/src/lsqcommit_agent_agent_driver.sv", input: "lsqcommit xaction", output: "pendingPtr/flushSb 接口驱动", purpose: "真实驱动 commit 端口。", role: "把 TB 选择的 commit pointer 送到 DUT。" }),
    "lsq_commit_handler::mark_rob_commit_batch": fn("lsq_commit_handler::mark_rob_commit_batch", { file: "lsq_commit_handler.sv", input: "uids[$]", output: "rob_commit 状态", purpose: "发送 commit 后更新软件状态。", role: "ROB commit 和 LQ/SQ deq 都完成后才 retire。", calls: ["lsq_commit_handler::mark_rob_commit_uid"] }),
    "lsq_commit_handler::mark_rob_commit_uid": fn("lsq_commit_handler::mark_rob_commit_uid", { file: "lsq_commit_handler.sv", input: "uid", output: "rob_commit=1", purpose: "标记单个 uid 已被 pendingPtr 覆盖。", role: "如果 LQ/SQ active map 已释放，则立即尝试 retire。", calls: ["common_data_transaction::try_retire_committed_uid"] }),
    "lsq_commit_handler::apply_raw_ctrl_deq": fn("lsq_commit_handler::apply_raw_ctrl_deq", { file: "lsq_commit_handler.sv", input: "lq_count,lq_ptr,sq_count,sq_ptr", output: "LQ/SQ active map 释放", purpose: "处理 DUT ctrl monitor 采到的 deq。", role: "deq 的最终真源来自 DUT monitor，而不是本地预测。", calls: ["lsq_commit_handler::apply_dut_lq_deq", "lsq_commit_handler::apply_dut_sq_deq"] }),
    "lsq_commit_handler::report_deq_mismatch": fn("lsq_commit_handler::report_deq_mismatch", { file: "lsq_commit_handler.sv", input: "msg", output: "fatal 或 warning", purpose: "报告 DUT deq 与软件模型不一致。", role: "帮助定位 LQ/SQ free count 或 pointer 漂移，resync 开关打开时可降级继续 debug。" }),
    "lsq_commit_handler::apply_dut_lq_deq": fn("lsq_commit_handler::apply_dut_lq_deq", { file: "lsq_commit_handler.sv", input: "count,deq_ptr", output: "LQ 释放", purpose: "释放 DUT 已出队的 LQ 项。", role: "按 lq key 反查 active uid，释放映射和本地 free count。", calls: ["common_data_transaction::lookup_active_uid_by_lq", "common_data_transaction::release_uid_lq_mapping", "common_data_transaction::try_retire_committed_uid", "lsq_commit_handler::report_deq_mismatch"] }),
    "lsq_commit_handler::apply_dut_sq_deq": fn("lsq_commit_handler::apply_dut_sq_deq", { file: "lsq_commit_handler.sv", input: "count,deq_ptr", output: "SQ 释放", purpose: "释放 DUT 已出队的 SQ 项。", role: "按 sq key 反查 active uid，释放映射和本地 free count。", calls: ["common_data_transaction::lookup_active_uid_by_sq", "common_data_transaction::release_uid_sq_mapping", "common_data_transaction::try_retire_committed_uid", "lsq_commit_handler::report_deq_mismatch"] }),
    "common_data_transaction::lookup_active_uid_by_lq": fn("common_data_transaction::lookup_active_uid_by_lq", { file: "common_data_transaction.sv", input: "lq key", output: "uid", purpose: "LQ event/deq 反查 active uid。", role: "monitor 不直接携带 uid 时靠 active map 定位。" }),
    "common_data_transaction::lookup_active_uid_by_sq": fn("common_data_transaction::lookup_active_uid_by_sq", { file: "common_data_transaction.sv", input: "sq key", output: "uid", purpose: "SQ event/deq 反查 active uid。", role: "store feedback/deq 定位依赖该映射。" }),
    "common_data_transaction::release_uid_lq_mapping": fn("common_data_transaction::release_uid_lq_mapping", { file: "common_data_transaction.sv", input: "uid", output: "active_lq_mapped=0", purpose: "释放 uid 的 LQ active map。", role: "LQ deq 完成后 uid 才能满足 retire 条件。" }),
    "common_data_transaction::release_uid_sq_mapping": fn("common_data_transaction::release_uid_sq_mapping", { file: "common_data_transaction.sv", input: "uid", output: "active_sq_mapped=0", purpose: "释放 uid 的 SQ active map。", role: "SQ deq 完成后 uid 才能满足 retire 条件。" }),
    "common_data_transaction::try_retire_committed_uid": fn("common_data_transaction::try_retire_committed_uid", { file: "common_data_transaction.sv", input: "uid", output: "可能 success/active=0", purpose: "尝试退休已 commit 且 LSQ deq 完的 uid。", role: "ROB commit 与 LQ/SQ active map 释放都满足后才结束 uid 生命周期。", calls: ["common_data_transaction::retire_active_uid"] }),
    "common_data_transaction::retire_active_uid": fn("common_data_transaction::retire_active_uid", { file: "common_data_transaction.sv", input: "uid", output: "active map 删除、success=1", purpose: "uid 生命周期结束清理。", role: "删除 ROB/LQ/SQ active map，并兜底清 issue queue 残留。", calls: ["common_data_transaction::remove_uid_from_issue_queues"] }),
    "common_data_transaction::remove_uid_from_issue_queues": fn("common_data_transaction::remove_uid_from_issue_queues", { file: "common_data_transaction.sv", input: "uid", output: "相关 issue queue 项删除", purpose: "按 uid 清理队列残留。", role: "正常发射后已删除，这里是 flush/retire/replay 场景的兜底一致性清理。" }),

    "exception_redirect_replay_task": fn("exception_redirect_replay_task", { file: "memblock_dispatch_base_sequence.sv", input: "feedback_event_q", output: "replay/redirect/fault 状态推进", purpose: "主控循环中的复杂事件处理入口。", role: "把 writeback handler 推入的复杂事件交给专门 handler。", calls: ["exception_redirect_replay_handler::process_pending_events"] }),
    "exception_redirect_replay_handler::process_pending_events": fn("exception_redirect_replay_handler::process_pending_events", {
      file: "exception_redirect_replay_handler.sv",
      input: "feedback_event_q, ptw_wait_replay_q, active redirect",
      output: "redirect drive/replay pending/fault 状态",
      purpose: "统一处理 backend replay、redirect 和 fault。",
      role: "无 active redirect 时释放 ready 的 PTW wait replay；active redirect 期间 route/issue 冻结，PTW wait replay 保持等待。新事件里 redirect 优先级高于 replay/fault。",
      calls: ["service_ptw_wait_replay", "advance_active_redirect", "select_oldest_redirect", "redirect_from_event", "common_data_transaction::request_redirect_flush", "common_data_transaction::push_redirect_drive", "exception_redirect_replay_handler::requeue_events_not_flushed_by_redirect", "handle_replay_event", "handle_fault_event"],
      source: String.raw`task process_pending_events();
    service_ptw_wait_replay();
    advance_active_redirect();
    if (data.active_redirect.valid) begin
        return;
    end

    while (data.pop_feedback_event(wb_event)) begin
        events.push_back(wb_event);
    end

    if (select_oldest_redirect(events, redirect_event)) begin
        redirect = redirect_from_event(redirect_event);
        data.request_redirect_flush(redirect);
        data.push_redirect_drive(redirect);
        requeue_events_not_flushed_by_redirect(events, redirect);
        return;
    end

    foreach (events[idx]) begin
        if (event_is_replay(events[idx])) handle_replay_event(events[idx]);
        else if (event_is_fault(events[idx])) handle_fault_event(events[idx]);
    end
endtask:process_pending_events`,
      logicNotes: [
        "redirect 优先于 replay/fault。先选择最老 redirect，然后冻结 dispatch 并排队等待 redirect sequence 驱动。",
        "active redirect 期间 service_ptw_wait_replay 直接返回，不会把等待 replay 先转换成 replay_pending。",
        "同一批事件中未被 redirect flush 覆盖的非 redirect 事件会重新排回 exception_event_q，下一轮继续处理；被覆盖的事件直接丢弃。",
      ],
    }),
    "service_ptw_wait_replay": fn("service_ptw_wait_replay", {
      file: "exception_redirect_replay_handler.sv",
      input: "ptw_wait_replay_q, active_redirect",
      output: "无 active redirect 时 ready replay 重新 pending；active redirect 时保持等待",
      purpose: "释放等待 TLB response 的 replay。",
      role: "PTW-back replay 需要等 L2TLB response done 后再重新入队；但 active redirect 期间 route/issue 整体冻结，所以不释放 wait replay，避免先置 replay_pending 再被 redirect flush 清掉。",
      calls: ["common_data_transaction::pop_ready_ptw_wait_replay", "common_data_transaction::mark_replay_pending"],
      source: String.raw`function void service_ptw_wait_replay();
    memblock_ptw_wait_replay_t wait_item;
    bit timed_out;

    ensure_data();
    if (data.active_redirect.valid) begin
        // active redirect期间route/issue整体冻结，PTW wait replay保持等待，避免先置replay_pending再被flush清掉。
        return;
    end
    while (data.pop_ready_ptw_wait_replay(seq_csr_common::get_replay_wait_ptw_timeout(),
                                          wait_item,
                                          timed_out)) begin
        if (timed_out) begin
            ` + "`" + String.raw`uvm_warning("EXC_REDIRECT",
                         $sformatf("release ptw_wait_replay by timeout uid=%0d target=%0d replay_seq=%0d",
                                   wait_item.uid,
                                   wait_item.target,
                                   wait_item.replay_seq))
        end
        void'(data.mark_replay_pending(wait_item.uid,
                                       wait_item.target,
                                       wait_item.issue_epoch,
                                       wait_item.replay_seq,
                                       memblock_sync_pkg::get_dispatch_service_cycle()));
    end
endfunction:service_ptw_wait_replay`,
      logicNotes: [
        "该函数现在采用保守策略：只要 active_redirect.valid 为高，就不释放任何 PTW wait replay。",
        "这样可以避免 redirect recovery 期间出现短暂 replay_pending 中间态；被 redirect 覆盖的等待项最终会由 clear_ptw_wait_replay_by_redirect 兜底清理。",
      ],
    }),
    "advance_active_redirect": fn("advance_active_redirect", { file: "exception_redirect_replay_handler.sv", input: "active redirect state", output: "flush 可能完成", purpose: "推进已启动 redirect 的 recovery。", role: "等待 redirect drive done 后应用 flush，并解除 flush_in_progress。", calls: ["common_data_transaction::redirect_drive_done_for", "common_data_transaction::apply_redirect_flush"] }),
    "select_oldest_redirect": fn("select_oldest_redirect", { file: "exception_redirect_replay_handler.sv", input: "redirect events[$]", output: "最老 redirect", purpose: "多个 redirect 中选择 ROB 最老项。", role: "避免较新的 redirect 先 flush 导致旧异常处理错序。", calls: ["exception_redirect_replay_handler::event_is_redirect", "redirect_event_is_older"] }),
    "redirect_from_event": fn("redirect_from_event", {
      file: "exception_redirect_replay_handler.sv",
      input: "memblock_wb_event_t redirect event",
      output: "memblock_redirect_payload_t",
      purpose: "从统一 wb_event 中取出 redirect payload。",
      role: "保证 redirect event 必须携带 redirect.valid 的完整 payload，后续 request_redirect_flush 和 redirect sequence 都使用这个 payload。",
      source: String.raw`function memblock_redirect_payload_t redirect_from_event(input memblock_wb_event_t wb_event);
    if (wb_event.redirect.valid) begin
        return wb_event.redirect;
    end
    ` + "`" + String.raw`uvm_fatal("EXC_REDIRECT", "redirect wb_event requires redirect.valid payload")
endfunction:redirect_from_event`,
    }),
    "redirect_event_is_older": fn("redirect_event_is_older", { file: "exception_redirect_replay_handler.sv", input: "candidate,current", output: "bit", purpose: "比较 redirect ROB 年龄。", role: "基于 rob_order_util，不能用普通 int 大小比较。", calls: ["redirect_from_event"] }),
    "exception_redirect_replay_handler::event_is_redirect": fn("exception_redirect_replay_handler::event_is_redirect", { file: "exception_redirect_replay_handler.sv", input: "wb_event", output: "bit", purpose: "判断复杂事件是否为 redirect。", role: "和 writeback handler 一样校验 redirect_valid 与 redirect.valid 一致，保证 payload 语义唯一。" }),
    "exception_redirect_replay_handler::event_is_replay": fn("exception_redirect_replay_handler::event_is_replay", { file: "exception_redirect_replay_handler.sv", input: "wb_event", output: "bit", purpose: "判断复杂事件是否为 backend replay。", role: "用于 redirect 优先处理完成后分派 replay 事件。" }),
    "exception_redirect_replay_handler::event_is_fault": fn("exception_redirect_replay_handler::event_is_fault", { file: "exception_redirect_replay_handler.sv", input: "wb_event", output: "bit", purpose: "判断复杂事件是否为 fault。", role: "用于 redirect/replay 之外的异常状态更新。" }),
    "exception_redirect_replay_handler::event_should_wait_ptw": fn("exception_redirect_replay_handler::event_should_wait_ptw", {
      file: "exception_redirect_replay_handler.sv",
      input: "wb_event.ptw_back_replay 和 plus 开关",
      output: "bit",
      purpose: "判断 replay 是否需要等待 L2TLB/PTW response。",
      role: "PTW-back replay 不能立即重新入队，需要先进入 ptw_wait_replay_q，等 TLB response 回填 uid record 后再 mark_replay_pending。",
      source: String.raw`function bit event_should_wait_ptw(input memblock_wb_event_t wb_event);
    if (!seq_csr_common::is_initialized() ||
        !seq_csr_common::get_replay_wait_ptw_en()) begin
        return 1'b0;
    end
    return wb_event.ptw_back_replay;
endfunction:event_should_wait_ptw`,
    }),
    "exception_redirect_replay_handler::requeue_events_not_flushed_by_redirect": fn("exception_redirect_replay_handler::requeue_events_not_flushed_by_redirect", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq_help/exception_redirect_replay_handler.sv",
      input: "同一批 events[$], redirect payload",
      output: "未被 flush 的非 redirect 事件 push_front 回 exception_event_q",
      purpose: "redirect 选中后，保留不在 flush 范围内的复杂事件。",
      role: "redirect 优先处理时不能简单丢弃同批 replay/fault；该函数用 ROB flush 规则过滤，只有被 redirect 覆盖的旧事件丢弃，其余事件下一轮继续处理。",
      calls: ["exception_redirect_replay_handler::event_is_redirect", "rob_order_util::rob_need_flush"],
      source: String.raw`function void requeue_events_not_flushed_by_redirect(input memblock_wb_event_t events[$],
                                                     input memblock_redirect_payload_t redirect);
    for (int idx = events.size(); idx > 0; idx--) begin
        wb_item = events[idx - 1];
        if (event_is_redirect(wb_item)) begin
            continue;
        end
        if (wb_item.has_rob &&
            rob_order_util::rob_need_flush(wb_item.rob_key, redirect)) begin
            continue;
        end
        data.exception_event_q.push_front(wb_item);
    end
endfunction:requeue_events_not_flushed_by_redirect`,
      logicNotes: [
        "倒序 push_front 是为了尽量保持原始事件相对顺序。",
        "这解决的是同一 service cycle 中 redirect 与其他复杂事件并存的问题：redirect 先执行，但不该让未被 flush 的事件丢失。",
      ],
    }),
    "common_data_transaction::request_redirect_flush": fn("common_data_transaction::request_redirect_flush", { file: "common_data_transaction.sv", input: "redirect payload", output: "flush_in_progress=1, flush_epoch++", purpose: "启动 redirect recovery。", role: "冻结 admission/issue/commit，并把 redirect payload 交给 redirect sequence 驱动。", calls: ["common_data_transaction::push_redirect_drive"] }),
    "common_data_transaction::push_redirect_drive": fn("common_data_transaction::push_redirect_drive", { file: "common_data_transaction.sv", input: "redirect payload", output: "redirect_drive_q.push_back", purpose: "排队等待 redirect driver 发送。", role: "让 exception handler 与 redirect_agent sequence 解耦。" }),
    "memblock_redirect_dispatch_base_sequence::body": fn("memblock_redirect_dispatch_base_sequence::body", { file: "memblock_redirect_dispatch_base_sequence.sv", input: "default sequence 启动", output: "持续驱动 redirect 或 idle", purpose: "redirect_agent 的真实驱动 sequence。", role: "从 common_data 取 pending redirect payload 并驱动到 DUT。", calls: ["common_data_transaction::try_pop_redirect_drive", "drive_redirect_payload", "drive_idle_once"] }),
    "common_data_transaction::try_pop_redirect_drive": fn("common_data_transaction::try_pop_redirect_drive", { file: "common_data_transaction.sv", input: "redirect_drive_q", output: "payload", purpose: "取一个待驱动 redirect。", role: "redirect sequence 的输入来源。" }),
    "drive_redirect_payload": fn("drive_redirect_payload", { file: "memblock_redirect_dispatch_base_sequence.sv", input: "redirect payload", output: "redirect_agent xaction", purpose: "真实驱动 redirect 接口。", role: "驱动完成后通知 common_data，exception handler 下一轮才能 apply flush。", calls: ["assign_redirect_xaction", "redirect_agent_driver::send_pkt", "common_data_transaction::mark_redirect_drive_done"] }),
    "assign_redirect_xaction": fn("assign_redirect_xaction", { file: "memblock_redirect_dispatch_base_sequence.sv", input: "payload", output: "redirect xaction fields", purpose: "把软件 redirect payload 转成 agent transaction。", role: "填 robIdx、level、cfiUpdate 等 DUT 接口字段。" }),
    "redirect_agent_driver::send_pkt": fn("redirect_agent_driver::send_pkt", { file: "mem_ut/ver/ut/memblock/agent/redirect_agent_agent/src/redirect_agent_agent_driver.sv", input: "redirect xaction", output: "DUT redirect interface fire", purpose: "真实驱动 redirect 接口。", role: "形成 DUT 侧 flush/recovery 触发。" }),
    "common_data_transaction::mark_redirect_drive_done": fn("common_data_transaction::mark_redirect_drive_done", { file: "common_data_transaction.sv", input: "payload", output: "redirect drive done 标记", purpose: "通知 recovery 可进入 apply flush。", role: "避免同一 delta 内边发射边清状态造成竞争。" }),
    "common_data_transaction::redirect_drive_done_for": fn("common_data_transaction::redirect_drive_done_for", { file: "common_data_transaction.sv", input: "payload", output: "bit", purpose: "查询 redirect 是否已驱动。", role: "exception handler 用它判断是否能应用 flush。" }),
    "common_data_transaction::apply_redirect_flush": fn("common_data_transaction::apply_redirect_flush", {
      file: "common_data_transaction.sv",
      input: "redirect payload",
      output: "被 flush uid 准备重入队；全局 flush 状态解除",
      purpose: "应用 redirect flush。",
      role: "先按 ROB 范围清理需要重发的 uid，再清 PTW wait replay 和 redirect drive 队列，最后解除 dispatch_flush_in_progress/issue_freeze_ack。",
      sideEffects: ["flush_in_progress=0", "dispatch_flush_in_progress=0", "issue_freeze_ack=0", "active_redirect 清零"],
      calls: ["common_data_transaction::apply_redirect_flush_range", "common_data_transaction::clear_ptw_wait_replay_by_redirect", "common_data_transaction::clear_redirect_drive_queue"],
      source: String.raw`function void apply_redirect_flush(input memblock_redirect_payload_t redirect);
    if (!redirect.valid) begin
        ` + "`" + String.raw`uvm_fatal("COMMON_DATA", "apply_redirect_flush requires valid redirect")
    end
    apply_redirect_flush_range(redirect);
    clear_ptw_wait_replay_by_redirect(redirect);
    clear_redirect_drive_queue();
    redirect_phase = MEMBLOCK_REDIRECT_PHASE_STATE_FLUSH_APPLIED;
    flush_in_progress  = 1'b0;
    memblock_sync_pkg::dispatch_flush_in_progress = 1'b0;
    issue_freeze_ack   = 1'b0;
    active_redirect    = '{default:'0};
    redirect_phase     = MEMBLOCK_REDIRECT_PHASE_IDLE;
endfunction:apply_redirect_flush`,
      logicNotes: [
        "该函数不是一收到 redirect 就清状态，而是在 redirect driver 已经完成后由 advance_active_redirect 调用，避免同一仿真 delta 内边驱动边清理。",
        "清状态的核心在 apply_redirect_flush_range；本函数负责把 recovery 收尾并释放全局发射冻结。",
      ],
    }),
    "common_data_transaction::apply_redirect_flush_range": fn("common_data_transaction::apply_redirect_flush_range", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv",
      input: "redirect payload",
      output: "redirect 覆盖范围内 uid 被 prepare_uid_for_redirect_reissue；max_enqueued_uid 可能回退",
      purpose: "按 ROB 顺序语义找出 redirect 需要 flush 并重新发射的 uid。",
      role: "只扫描 [terminal_done_uid, max_enqueued_uid+1) 活跃窗口，避免 10 万笔全表扫描；真正是否 flush 由 rob_order_util::rob_need_flush 判断。",
      calls: ["common_data_transaction::advance_terminal_done_uid", "common_data_transaction::get_active_scan_begin_uid", "common_data_transaction::get_active_scan_end_uid", "rob_order_util::rob_need_flush", "common_data_transaction::prepare_uid_for_redirect_reissue", "common_data_transaction::rollback_max_enqueued_uid"],
      source: String.raw`function void apply_redirect_flush_range(input memblock_redirect_payload_t redirect);
    advance_terminal_done_uid();
    begin_uid = get_active_scan_begin_uid();
    end_uid   = get_active_scan_end_uid();
    found_flushed = 1'b0;

    // redirect flush只扫描已admission的活跃窗口；真正flush判断仍由ROB顺序语义决定。
    for (memblock_uid_t uid = begin_uid; uid < end_uid; uid++) begin
        status = get_status(uid);
        if (status.success || (!status.active && !status.writeback && !status.pass)) begin
            continue;
        end
        rob_key = status.get_rob_key();
        if (rob_order_util::rob_need_flush(rob_key, redirect)) begin
            if (!found_flushed || uid < oldest_flushed_uid) begin
                oldest_flushed_uid = uid;
                found_flushed = 1'b1;
            end
            prepare_uid_for_redirect_reissue(uid, redirect);
        end
    end
    if (found_flushed) begin
        rollback_max_enqueued_uid(oldest_flushed_uid);
    end
endfunction:apply_redirect_flush_range`,
      logicNotes: [
        "begin/end 来自公共进度表：已经连续 terminal_done 的 uid 不再扫描，尚未 admission 的 uid 也不扫描。",
        "oldest_flushed_uid 用来回退 admission 边界。回退后 LSQ admission 会从这条 uid 重新顺序入队，而不是继续发新 uid。",
      ],
    }),
    "common_data_transaction::prepare_uid_for_redirect_reissue": fn("common_data_transaction::prepare_uid_for_redirect_reissue", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv",
      input: "uid, redirect payload",
      output: "uid 状态清成可重新 admission；pending_lq/sq_cancel_count 增加；dynamic_epoch++",
      purpose: "把被 redirect flush 的 uid 从旧动态实例切换到待重发状态。",
      role: "清 active map、issue queue、writeback/pass/commit/deq 结果，并保留 redirect_pending/flushed 标记，让同 uid 后续重新 admission。",
      sideEffects: ["可能 retire_active_uid/remove_uid_from_issue_queues", "pending_lq_cancel_count/pending_sq_cancel_count 累加", "dynamic_epoch++", "redirect_pending=1", "flushed=1"],
      calls: ["common_data_transaction::retire_active_uid", "common_data_transaction::remove_uid_from_issue_queues", "common_data_transaction::clear_uid_dispatch_result"],
      source: String.raw`function void prepare_uid_for_redirect_reissue(input memblock_uid_t uid,
                                               input memblock_redirect_payload_t redirect);
    status = get_status(uid);
    if (status.success) begin
        ` + "`" + String.raw`uvm_fatal("COMMON_DATA",
                   $sformatf("redirect tries to flush already success uid=%0d", uid))
    end
    main_tr = get_main_transaction(uid);
    had_lq_mapping = status.active_lq_mapped;
    had_sq_mapping = status.active_sq_mapped;
    // redirect命中的旧动态实例不再等待writeback/commit；清queue/map后等待同uid重新admission。
    if (status.active) begin
        retire_active_uid(uid);
    end else begin
        remove_uid_from_issue_queues(uid);
    end
    if (had_lq_mapping) begin
        pending_lq_cancel_count += main_tr.numLsElem;
    end
    if (had_sq_mapping) begin
        pending_sq_cancel_count += main_tr.numLsElem;
    end
    clear_uid_dispatch_result(uid);
    status.redirect_pending = 1'b1;
    status.flushed          = 1'b1;
    status.dynamic_epoch++;
    status.active           = 1'b0;
    status.success          = 1'b0;
endfunction:prepare_uid_for_redirect_reissue`,
      logicNotes: [
        "这里的 reissue 不是创建新 uid，而是同一个 uid 重新 admission/issue。dynamic_epoch 用来记录这个 uid 已进入下一轮动态实例。",
        "pending cancel count 是给 LSQ admission sequence 的恢复输入；真正回滚 lsq_ctrl 在 apply_pending_lsq_cancels 中执行。",
      ],
    }),
    "rob_order_util::rob_need_flush": fn("rob_order_util::rob_need_flush", { file: "rob_order_util.sv", input: "uop_rob, redirect payload", output: "bit", purpose: "判断某个 ROB key 是否被 redirect 覆盖。", role: "统一处理 ROB flag/value 回绕语义，避免用普通整数比较导致 flush 范围错误。" }),
    "common_data_transaction::clear_redirect_drive_queue": fn("common_data_transaction::clear_redirect_drive_queue", { file: "common_data_transaction.sv", input: "pending_redirect_drive_q", output: "redirect drive 队列和 inflight 状态清空", purpose: "redirect recovery 收尾清理。", role: "防止旧 redirect payload 在 flush 完成后再次被 redirect sequence 驱动。" }),
    "common_data_transaction::clear_uid_dispatch_result": fn("common_data_transaction::clear_uid_dispatch_result", {
      file: "common_data_transaction.sv",
      input: "uid",
      output: "dispatch/writeback/pass/fault/commit/deq 状态清零，issue_killed=1",
      purpose: "清掉被 flush uid 的旧动态实例结果。",
      role: "避免 redirect 后旧 writeback/pass/commit 继续参与 success 判断；随后 prepare_uid_for_redirect_reissue 会设置 redirect_pending/flushed 等待重入队。",
      source: String.raw`function void clear_uid_dispatch_result(input memblock_uid_t uid);
    status = get_status(uid);
    status.enq             = 1'b0;
    status.issue_ready     = 1'b0;
    status.queued_load     = 1'b0;
    status.queued_sta      = 1'b0;
    status.queued_std      = 1'b0;
    status.load_dispatched = 1'b0;
    status.sta_dispatched  = 1'b0;
    status.std_dispatched  = 1'b0;
    status.load_writeback  = 1'b0;
    status.sta_writeback   = 1'b0;
    status.std_writeback   = 1'b0;
    status.writeback       = 1'b0;
    status.pass            = 1'b0;
    status.issue_killed    = 1'b1;
    status.replay_pending  = 1'b0;
    status.redirect_pending = 1'b0;
    status.rob_commit      = 1'b0;
    status.lsq_deq         = 1'b0;
    status.success         = 1'b0;
endfunction:clear_uid_dispatch_result`,
    }),
    "common_data_transaction::clear_ptw_wait_replay_by_redirect": fn("common_data_transaction::clear_ptw_wait_replay_by_redirect", { file: "common_data_transaction.sv", input: "redirect payload", output: "被 flush 的 PTW wait replay 删除", purpose: "清理 redirect 范围内的等待 replay。", role: "防止 flush 后旧 replay 再次入队。" }),
    "handle_replay_event": fn("handle_replay_event", { file: "exception_redirect_replay_handler.sv", input: "replay wb_event", output: "replay_pending 或 PTW wait", purpose: "处理 backend replay。", role: "解析 uid/target/epoch，PTW-back replay 可先等待 TLB response。", calls: ["common_data_transaction::resolve_uid_for_event", "common_data_transaction::push_ptw_wait_replay", "common_data_transaction::mark_replay_pending"] }),
    "common_data_transaction::resolve_uid_for_event": fn("common_data_transaction::resolve_uid_for_event", { file: "common_data_transaction.sv", input: "wb_event uid/ROB/LQ/SQ key", output: "uid", purpose: "把事件定位到 active uid。", role: "所有 monitor event 最终都要转成 uid 才能更新状态。" }),
    "common_data_transaction::push_ptw_wait_replay": fn("common_data_transaction::push_ptw_wait_replay", { file: "common_data_transaction.sv", input: "uid,target,event", output: "ptw_wait_replay_q", purpose: "保存等待 TLB response 的 replay。", role: "避免 PTW response 未回来时提前重发。" }),
    "common_data_transaction::pop_ready_ptw_wait_replay": fn("common_data_transaction::pop_ready_ptw_wait_replay", { file: "common_data_transaction.sv", input: "timeout", output: "ready replay item", purpose: "取出已满足 TLB response 条件的 replay。", role: "重新进入 mark_replay_pending 前的等待解除。" }),
    "common_data_transaction::mark_replay_pending": fn("common_data_transaction::mark_replay_pending", { file: "common_data_transaction.sv", input: "uid,target,issue_epoch,replay_seq", output: "replay_pending 和 target mask", purpose: "进入 backend replay 重发状态。", role: "清旧 issue queue、清目标结果、bump replay_seq，再等待 route_uid 重新入队。", calls: ["common_data_transaction::remove_uid_from_issue_queues", "common_data_transaction::bump_replay_seq"] }),
    "common_data_transaction::bump_replay_seq": fn("common_data_transaction::bump_replay_seq", { file: "common_data_transaction.sv", input: "uid", output: "replay_seq++", purpose: "生成新的 replay 版本。", role: "过滤旧队列项和旧反馈。" }),
    "handle_fault_event": fn("handle_fault_event", { file: "exception_redirect_replay_handler.sv", input: "fault wb_event", output: "消费 fault recovery event", purpose: "消费异常 fault recovery 队列事件。", role: "fault target 状态已由 writeback_status_handler 首次落表，这里只解析 uid/epoch/replay_seq 并保留调试上下文。" }),
    "drive_idle_once": fn("drive_idle_once", { file: "memblock_redirect_dispatch_base_sequence.sv", input: "无 pending redirect", output: "idle xaction", purpose: "redirect sequence 空闲驱动。", role: "保持 agent 接口稳定。" }),

    "all_transactions_terminal_done": fn("all_transactions_terminal_done", { file: "memblock_main_dispatch_auto_build_main_table_base_sequence.sv", input: "common_data 状态", output: "bit", purpose: "判断测试是否闭环完成。", role: "要求 terminal_done_uid 达到 main_trans_num，且 issue queue/active map/pending redirect/flushSb/PTW wait 都为空。" }),
    "common_data_transaction::end_test_check": fn("common_data_transaction::end_test_check", { file: "common_data_transaction.sv", input: "所有公共表/队列", output: "fatal 或通过", purpose: "测试末尾一致性检查。", role: "关闭 monitor capture，检查 active uid、pending event、issue queue、raw queue 残留。" }),
  };

  const flowDefinitions = {
    overview: {
      title: "端到端 real smoke 总览",
      summary: "从 testcase/env 启动到主表构建、LSQ admission、TLB/L2TLB、issue 发射、monitor/writeback、commit/deq、replay/redirect 和 end check 的全局调用关系。",
      stages: [
        stage("启动", "UVM/testcase 建环境并启动主控与并行 agent sequence。", [
          lane("testcase", ["tc_base::build_phase", "tc_dispatch_real_smoke::main_phase", "run_real_smoke_sequence"]),
          lane("env", ["memblock_env::build_phase", "memblock_env::connect_phase", "uvm_default_sequence_start"]),
        ]),
        stage("主控", "主控 sequence 构建事实源，并周期性服务状态机。", [
          lane("main sequence", ["memblock_main_dispatch_auto_build_main_table_base_sequence::body", "memblock_dispatch_base_sequence::pre_body", "build_main_table", "service_real_dispatch_flow"]),
          lane("service loop", ["service_monitor_once", "route_all_issue_queues", "all_transactions_terminal_done", "common_data_transaction::end_test_check"]),
        ]),
        stage("并行接口流", "agent default sequence 并行驱动 DUT 接口并通过 common_data 协同。", [
          lane("admission", ["memblock_lsqenq_dispatch_base_sequence::body", "send_lsqenq_cycle", "memblock_lsqenq_dispatch_base_sequence::apply_pending_lsq_cancels", "complete_admission"]),
          lane("issue", ["memblock_issue_dispatch_base_sequence::body", "send_issue_cycle", "issue_field_assigner::assign_issue_item_fields", "issue_queue_scheduler::mark_issue_fire", "submit_issue_accept_pass"]),
          lane("commit", ["memblock_lsqcommit_dispatch_base_sequence::body", "send_lsqcommit_cycle", "lsq_commit_handler::apply_raw_ctrl_deq"]),
          lane("l2tlb", ["memblock_l2tlb_base_sequence::body", "send_l2tlb_cycle", "common_data_transaction::get_or_create_tlb_entry_by_req"]),
          lane("recovery", ["exception_redirect_replay_handler::process_pending_events", "exception_redirect_replay_handler::requeue_events_not_flushed_by_redirect", "memblock_redirect_dispatch_base_sequence::body", "common_data_transaction::apply_redirect_flush"]),
        ]),
      ],
    },
    main_table: {
      title: "主表构建与激励生成",
      summary: "主表是整个测试框架的控制表，uid 是所有后续表和状态的统一索引；该流程展示随机/手动主表如何进入 common_data。",
      stages: [
        stage("入口与初始化", "选择主表来源并清公共表。", [
          lane("entry", ["memblock_main_dispatch_auto_build_main_table_base_sequence::body", "build_main_table"]),
          lane("data", ["common_data_transaction::reset_all_tables", "common_data_transaction::alloc_uid"]),
        ]),
        stage("生成字段", "生成主表项并修正为最小合法 transaction。", [
          lane("random", ["build_random_main_table", "randomize_main_transaction", "select_op_class_by_weight"]),
          lane("template", ["apply_minimal_op_template", "random_load_fuoptype", "random_store_fuoptype", "random_prefetch_fuoptype", "random_amo_fuoptype"]),
          lane("legalize", ["apply_legal_addr_template", "randomize_send_pri_value", "randomize_delay_value", "validate_main_table_entry"]),
          lane("manual", ["import_manual_main_table"]),
        ]),
        stage("生成期复用与收尾", "在主表生成期提高地址相关概率并初始化状态表。", [
          lane("addr reuse", ["choose_addr_ref_window", "prune_recent_uid_q", "apply_addr_reuse_window", "push_recent_uid"]),
          lane("status", ["init_status_for_main_table", "common_data_transaction::init_status_for_uid", "common_data_transaction::set_main_transaction", "common_data_transaction::check_main_table_complete"]),
        ]),
      ],
    },
    lsq_admission: {
      title: "LSQ admission 入队",
      summary: "LSQ admission sequence 从主表取 uid，结合本地 LSQ 模型和 DUT canAccept 决定是否入队；成功后激活 uid、生成 TLB 表并 route issue queue。",
      stages: [
        stage("sequence loop", "等待主表 ready 后进入 admission 主循环。", [
          lane("setup", ["memblock_lsqenq_dispatch_base_sequence::body", "memblock_lsqenq_dispatch_base_sequence::configure_from_plus", "wait_for_main_table", "drive_lsqenq_loop"]),
        ]),
        stage("一拍 admission", "选候选、填 slot、驱动 DUT、确认响应。", [
          lane("cycle", ["send_lsqenq_cycle", "memblock_lsqenq_dispatch_base_sequence::apply_pending_lsq_cancels", "admit_non_lsq_if_ready"]),
          lane("select", ["common_data_transaction::get_next_new_admit_uid", "collect_lsq_candidates", "lsq_ctrl_model::can_allocate", "lsq_ctrl_model::preview_allocate"]),
          lane("drive", ["assign_lsqenq_slot", "lsqenq_agent_driver::send_pkt", "lsqenq_agent_driver::wait_lsq_can_accept", "lsqenq_agent_driver::sample_lsqenq_resp"]),
          lane("commit", ["confirm_lsq_candidates", "lsq_ctrl_model::commit_allocate_with_resp", "complete_admission"]),
        ]),
        stage("入队后处理", "active/enq 在 commit_allocate_with_resp 内完成；complete_admission 继续做 CSR drain、TLB builder 和 issue route。", [
          lane("active/enq", ["lsq_ctrl_model::commit_allocate_with_resp", "common_data_transaction::set_status_field", "common_data_transaction::mark_uid_enqueued", "common_data_transaction::activate_uid_by_behavior"]),
          lane("tlb", ["complete_admission", "common_data_transaction::set_status_field", "issue_queue_scheduler::route_uid"]),
          lane("route", ["issue_queue_scheduler::route_uid"]),
        ]),
      ],
    },
    l2tlb: {
      title: "TLB 表与 L2TLB responder",
      summary: "L2TLB agent 代替的是 L2TLB 对上游 DTLB 的 responder；查表 key 来自 request vpn/s2xlate 和 runtime CSR 的 ASID/VMID。",
      stages: [
        stage("TLB 表生成", "L2TLB request 到来时按 by-key cache 查/建 TLB entry；没有 admission 后预构建 per-uid TLB 表。", [
          lane("builder", ["common_data_transaction::get_or_create_tlb_entry_by_req", "common_data_transaction::build_tlb_entry_for_key"]),
        ]),
        stage("responder sequence", "采 request，按 by-key entry 回包，并回填匹配 uid record。", [
          lane("setup", ["memblock_l2tlb_base_sequence::body", "memblock_l2tlb_base_sequence::ensure_context", "memblock_l2tlb_base_sequence::configure_from_plus", "wait_for_main_table", "drive_l2tlb_loop"]),
          lane("request", ["send_l2tlb_cycle", "memblock_l2tlb_base_sequence::request_valid", "sample_request_fields", "memblock_l2tlb_base_sequence::create_l2tlb_xaction"]),
          lane("lookup", ["common_data_transaction::get_or_create_tlb_entry_by_req", "common_data_transaction::build_tlb_entry_for_key"]),
          lane("response", ["memblock_l2tlb_base_sequence::choose_latency", "fill_dtlb_resp_from_entry", "common_data_transaction::update_uid_tlb_records_by_entry", "memblock_l2tlb_base_sequence::send_l2tlb_item"]),
        ]),
      ],
    },
    issue: {
      title: "LOAD/STA/STD 调度与发射",
      summary: "issue route 把 ready uid 放入 LOAD/STA/STD 队列；lintsissue sequence 按 send_pri 或并行策略选择候选，驱动 DUT 后只标记 fire 的 item。",
      stages: [
        stage("route", "从 status/main/TLB 表决定哪些 target 可入队。", [
          lane("main route", ["route_all_issue_queues", "issue_queue_scheduler::route_all_ready_uids", "issue_queue_scheduler::route_uid"]),
          lane("target", ["lsq_ctrl_model::derive_op_behavior", "issue_queue_scheduler::route_target", "common_data_transaction::push_issue_queue_item"]),
        ]),
        stage("select", "按优先级、ROB 年龄、pipe 数选择发射候选。", [
          lane("sequence", ["memblock_issue_dispatch_base_sequence::body", "drive_dispatch_issue_loop", "send_issue_cycle", "submit_issue_accept_pass"]),
          lane("scheduler", ["select_issue_candidates", "issue_queue_scheduler::select_issue_candidates", "select_target_candidates"]),
        ]),
        stage("assign/drive/mark", "填 payload、驱动 DUT、更新状态。", [
          lane("assign", ["assign_issue_items", "issue_field_assigner::assign_issue_item_fields", "assign_main_issue_fields", "assign_issue_dep_fields", "assign_backend_meta_fields"]),
          lane("drive", ["lintsissue_agent_driver::send_pkt", "dut_ready_fire_boundary"]),
          lane("mark", ["mark_fired_items", "issue_queue_scheduler::mark_issue_fire", "issue_queue_scheduler::mark_issue_fire_already_accepted", "submit_issue_accept_pass", "common_data_transaction::mark_issue_snapshot", "common_data_transaction::delete_issue_queue_entry", "common_data_transaction::clear_replay_target_after_fire"]),
        ]),
      ],
    },
    monitor_writeback: {
      title: "monitor raw event 与 writeback 状态更新",
      summary: "monitor 只采事实并 push raw queue；主控 service loop 通过 adapter 转换事件，再由 writeback handler 更新状态或推复杂事件。",
      stages: [
        stage("主控服务", "每个软件周期 drain raw queue。", [
          lane("service", ["service_real_dispatch_flow", "service_monitor_once", "memblock_sync_pkg::tick_dispatch_service_cycle"]),
          lane("runtime context", ["collect_runtime_context_events", "dispatch_monitor_event_adapter::drain_csr_events", "dispatch_monitor_event_adapter::drain_sfence_events"]),
        ]),
        stage("raw monitor", "monitor 采样只进入 raw queue，不直接改状态。", [
          lane("int wb", ["io_mem_to_ooo_int_wb_monitor::mon_data", "memblock_sync_pkg::push_raw_int_wb"]),
          lane("iq feedback", ["io_mem_to_ooo_iq_feedback_monitor::mon_data", "memblock_sync_pkg::push_raw_iq_feedback"]),
          lane("ctrl", ["io_mem_to_ooo_ctrl_monitor::mon_data", "memblock_sync_pkg::push_raw_ctrl"]),
          lane("csr", ["csr_ctrl_agent_agent_monitor::mon_data", "memblock_sync_pkg::push_raw_csr"]),
          lane("sfence", ["fence_agent_agent_monitor::mon_data", "memblock_sync_pkg::push_raw_sfence"]),
        ]),
        stage("writeback raw", "int_wb 和 IQ feedback 转换为 memblock_wb_event_t。", [
          lane("batch collect", ["collect_monitor_event_batch", "dispatch_monitor_event_adapter::collect_writeback_events_batch", "dispatch_monitor_event_adapter::collect_ctrl_redirect_events_batch"]),
          lane("convert", ["convert_raw_int_wb", "convert_raw_iq_feedback", "event_has_active_uid"]),
          lane("batch arbiter", ["dispatch_monitor_batch_handler::process_monitor_event_batch", "writeback_status_handler::handle_event"]),
        ]),
        stage("状态更新", "普通 pass 直接写状态，复杂事件入队。", [
          lane("normal pass", ["writeback_status_handler::handle_event", "common_data_transaction::normalize_feedback_event", "common_data_transaction::mark_target_normal_pass", "conditional_set_target_status_field", "required_targets_done"]),
          lane("complex", ["common_data_transaction::push_feedback_event"]),
        ]),
        stage("ctrl/redirect raw", "ctrl monitor 的 deq 和 memoryViolation 与 writeback 同批仲裁。", [
          lane("ctrl", ["dispatch_monitor_event_adapter::collect_ctrl_redirect_events_batch", "dispatch_monitor_event_adapter::apply_raw_ctrl_deq", "convert_raw_memory_violation"]),
        ]),
      ],
    },
    commit_deq: {
      title: "ROB commit 与 LQ/SQ deq",
      summary: "commit 端口由 TB 主动按 ROB 顺序驱动；LQ/SQ deq 以 DUT ctrl monitor 为真源。uid 只有 ROB commit 和对应 LQ/SQ active map 都释放后才 success。",
      stages: [
        stage("commit drive", "构造 pendingPtr 或 flushSb xaction。", [
          lane("sequence", ["memblock_lsqcommit_dispatch_base_sequence::body", "drive_lsqcommit_loop", "send_lsqcommit_cycle"]),
          lane("flushSb", ["request_scheduled_flushsb_if_due", "drive_flushsb_if_needed"]),
          lane("pendingPtr", ["lsq_commit_handler::build_lsqcommit_xaction", "lsq_commit_handler::advance_commit_cursor_past_done", "lsq_commit_handler::select_rob_commit_batch", "lsq_commit_handler::uid_is_commit_candidate", "lsqcommit_agent_driver::send_pkt"]),
        ]),
        stage("mark commit", "发送 pendingPtr 后更新 ROB commit 状态。", [
          lane("mark", ["lsq_commit_handler::mark_rob_commit_batch", "lsq_commit_handler::mark_rob_commit_uid", "common_data_transaction::try_retire_committed_uid"]),
        ]),
        stage("DUT deq", "ctrl monitor 返回 LQ/SQ deq 后释放 active map。", [
          lane("adapter", ["dispatch_monitor_event_adapter::apply_raw_ctrl_deq", "lsq_commit_handler::apply_raw_ctrl_deq"]),
          lane("lq", ["lsq_commit_handler::apply_dut_lq_deq", "common_data_transaction::lookup_active_uid_by_lq", "common_data_transaction::release_uid_lq_mapping"]),
          lane("sq", ["lsq_commit_handler::apply_dut_sq_deq", "common_data_transaction::lookup_active_uid_by_sq", "common_data_transaction::release_uid_sq_mapping"]),
          lane("mismatch", ["lsq_commit_handler::report_deq_mismatch"]),
        ]),
        stage("retire", "commit 与 deq 均完成后 uid 生命周期结束。", [
          lane("retire", ["common_data_transaction::try_retire_committed_uid", "common_data_transaction::retire_active_uid", "common_data_transaction::remove_uid_from_issue_queues"]),
        ]),
      ],
    },
    replay_redirect: {
      title: "backend replay 与 redirect/flush recovery",
      summary: "replay/redirect/fault 是复杂事件，不在 writeback handler 直接完成；统一进入 exception_redirect_replay_handler，redirect 会冻结发射并通过 redirect sequence 驱动 DUT。",
      stages: [
        stage("复杂事件入口", "writeback handler 把 replay/redirect/fault 推入 feedback_event_q。", [
          lane("event queue", ["common_data_transaction::push_feedback_event", "exception_redirect_replay_task", "exception_redirect_replay_handler::process_pending_events"]),
        ]),
        stage("redirect", "redirect 优先于 replay/fault，且选择 ROB 最老项。", [
          lane("select", ["select_oldest_redirect", "redirect_event_is_older", "redirect_from_event"]),
          lane("freeze", ["common_data_transaction::request_redirect_flush", "common_data_transaction::push_redirect_drive", "exception_redirect_replay_handler::requeue_events_not_flushed_by_redirect"]),
          lane("drive", ["memblock_redirect_dispatch_base_sequence::body", "common_data_transaction::try_pop_redirect_drive", "drive_redirect_payload", "assign_redirect_xaction", "redirect_agent_driver::send_pkt", "common_data_transaction::mark_redirect_drive_done"]),
          lane("apply", ["advance_active_redirect", "common_data_transaction::redirect_drive_done_for", "common_data_transaction::apply_redirect_flush", "common_data_transaction::apply_redirect_flush_range", "common_data_transaction::prepare_uid_for_redirect_reissue", "common_data_transaction::rollback_max_enqueued_uid", "common_data_transaction::clear_ptw_wait_replay_by_redirect"]),
        ]),
        stage("backend replay", "replay 清旧队列、bump 版本，再让 route_uid 重新入队。", [
          lane("handle", ["handle_replay_event", "exception_redirect_replay_handler::event_should_wait_ptw", "common_data_transaction::resolve_uid_for_event", "common_data_transaction::push_ptw_wait_replay"]),
          lane("ptw wait", ["service_ptw_wait_replay", "common_data_transaction::pop_ready_ptw_wait_replay"]),
          lane("pending", ["common_data_transaction::mark_replay_pending", "common_data_transaction::remove_uid_from_issue_queues", "common_data_transaction::bump_replay_seq"]),
          lane("refire", ["issue_queue_scheduler::route_uid", "issue_queue_scheduler::mark_issue_fire", "issue_queue_scheduler::mark_issue_fire_already_accepted", "common_data_transaction::clear_replay_target_after_fire"]),
        ]),
        stage("fault", "异常事件按策略进入 fault/redirect 处理。", [
          lane("fault", ["handle_fault_event"]),
          lane("idle", ["drive_idle_once"]),
        ]),
      ],
    },
    final_check: {
      title: "完成判定与状态管理",
      summary: "主控循环通过 all_transactions_terminal_done 判断是否完成；end_test_check 做最后一致性检查，防止 active map、issue queue、raw queue、pending event 残留。",
      stages: [
        stage("主循环完成条件", "所有 uid 都必须 terminal_done，且关键队列为空。", [
          lane("loop", ["service_real_dispatch_flow", "all_transactions_terminal_done"]),
          lane("queues", ["common_data_transaction::push_issue_queue_item", "common_data_transaction::delete_issue_queue_entry", "common_data_transaction::remove_uid_from_issue_queues"]),
        ]),
        stage("uid 生命周期", "uid 从主表创建到 active、issue、writeback、commit/deq、retire。", [
          lane("create", ["common_data_transaction::alloc_uid", "common_data_transaction::set_main_transaction", "common_data_transaction::init_status_for_uid"]),
          lane("active", ["common_data_transaction::activate_uid_by_behavior", "common_data_transaction::mark_uid_enqueued", "common_data_transaction::mark_issue_snapshot", "common_data_transaction::mark_target_normal_pass"]),
          lane("retire", ["lsq_commit_handler::mark_rob_commit_uid", "common_data_transaction::release_uid_lq_mapping", "common_data_transaction::release_uid_sq_mapping", "common_data_transaction::try_retire_committed_uid"]),
        ]),
        stage("end check", "测试末尾关闭采集并检查残留。", [
          lane("check", ["common_data_transaction::end_test_check"]),
        ]),
      ],
    },
  };

  const flowEdges = buildEdgesFromCatalog();

  function stage(title, desc, lanes) {
    return { title, desc, lanes };
  }

  function lane(title, nodes) {
    return { title, nodes };
  }

  function buildEdgesFromCatalog() {
    const edges = [];
    Object.values(functionCatalog).forEach((item) => {
      (item.calls || []).forEach((target) => {
        edges.push({
          from: item.id,
          to: target,
          type: functionCatalog[target] && functionCatalog[target].external ? "external" : "call",
        });
      });
    });
    return edges;
  }

  function escapeHtml(value) {
    return String(value)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function uniq(list) {
    return Array.from(new Set(list));
  }

  function nodesForFlow(flow) {
    return uniq(flow.stages.flatMap((s) => s.lanes.flatMap((l) => l.nodes)));
  }

  function incomingOf(id) {
    return flowEdges.filter((edge) => edge.to === id).map((edge) => edge.from);
  }

  function outgoingOf(id) {
    return (functionCatalog[id]?.calls || []).slice();
  }

  function relatedSet(id) {
    return new Set([id, ...incomingOf(id), ...outgoingOf(id)]);
  }

  function navigateToFunction(id, options = {}) {
    if (!id || !functionCatalog[id] || id === activeFunction) {
      return;
    }
    if (options.pushHistory !== false) {
      navigationStack.push(activeFunction);
    }
    activeFunction = id;
    focusMode = false;
    renderAll();
  }

  function goBackToParent() {
    const previous = navigationStack.pop();
    if (!previous || !functionCatalog[previous]) {
      return;
    }
    activeFunction = previous;
    focusMode = false;
    renderAll();
  }

  function directLogicSteps(id, meta) {
    const calls = outgoingOf(id);
    const steps = [];
    steps.push(`读取输入 ${meta.input || "无"}，进入 ${id} 的局部控制逻辑。`);
    if (meta.purpose) {
      steps.push(meta.purpose);
    }
    if (meta.role) {
      steps.push(`按当前流程职责执行：${meta.role}`);
    }
    if (meta.sideEffects && meta.sideEffects.length) {
      steps.push(`先写入关键副作用：${meta.sideEffects.join("；")}。`);
    }
    if (calls.length) {
      steps.push(`按顺序调用 ${calls.length} 个子函数：${calls.join(" -> ")}。`);
    } else {
      steps.push("该节点没有继续展开的子调用，通常是叶子 helper、状态写入点或 DUT/UVM 边界。");
    }
    steps.push(`完成后产出 ${meta.output || "无显式输出"}。`);
    return steps;
  }

  function renderPseudoCode(id, meta) {
    const calls = outgoingOf(id);
    const lines = [];
    const header = meta.kind && meta.kind !== "task/function" ? meta.kind : "function";
    lines.push(`${header} ${id}(${meta.input || "no input"})`);
    lines.push(`  // ${meta.purpose || "execute local logic"}`);
    if (meta.sideEffects && meta.sideEffects.length) {
      meta.sideEffects.forEach((effect) => lines.push(`  update_state("${effect}")`));
    }
    calls.forEach((call) => {
      lines.push(`  ${call}()`);
    });
    if (!calls.length) {
      lines.push("  apply_local_state_or_boundary_handshake()");
    }
    lines.push(`  return ${meta.output || "void"}`);
    lines.push("end");
    return lines.join("\n");
  }

  function explainCodeLine(line) {
    const trimmed = line.trim();
    if (!trimmed) {
      return "";
    }
    if (trimmed.startsWith("//")) {
      return "注释行概括该函数在流程中的直接目的。";
    }
    if (trimmed.startsWith("update_state")) {
      return "该行对应函数执行时会改写的共享状态、flag、队列或同步变量。";
    }
    if (trimmed.startsWith("return")) {
      return "该行对应函数完成后的输出、状态推进结果或边界握手结果。";
    }
    if (trimmed.endsWith("()")) {
      return "该行是可继续点击展开的内部子调用。";
    }
    if (trimmed === "end") {
      return "函数局部逻辑结束。";
    }
    return "函数入口，括号内是该目录记录的主要输入。";
  }

  function validateGraph() {
    const errors = [];
    Object.values(flowDefinitions).forEach((flow) => {
      nodesForFlow(flow).forEach((id) => {
        if (!functionCatalog[id]) {
          errors.push(`流程 ${flow.title} 引用了未定义函数: ${id}`);
        }
      });
    });
    Object.values(functionCatalog).forEach((item) => {
      (item.calls || []).forEach((target) => {
        if (!functionCatalog[target]) {
          errors.push(`函数 ${item.id} 调用了未定义函数: ${target}`);
        }
      });
    });
    return errors;
  }

  function maxDepth() {
    const value = depthSelect.value;
    return value === "all" ? Number.POSITIVE_INFINITY : Number(value);
  }

  function visibleByDepth(flow, id) {
    if (depthSelect.value === "all") {
      return true;
    }
    const roots = flow.stages.flatMap((s) => s.lanes.map((l) => l.nodes[0]).filter(Boolean));
    if (roots.includes(id)) {
      return true;
    }
    const limit = maxDepth();
    const queue = roots.map((root) => ({ id: root, depth: 0 }));
    const visited = new Set();
    while (queue.length) {
      const cur = queue.shift();
      if (cur.id === id && cur.depth <= limit) {
        return true;
      }
      if (visited.has(cur.id) || cur.depth >= limit) {
        continue;
      }
      visited.add(cur.id);
      outgoingOf(cur.id).forEach((next) => queue.push({ id: next, depth: cur.depth + 1 }));
    }
    return false;
  }

  function renderFlowOptions() {
    flowSelect.innerHTML = Object.entries(flowDefinitions)
      .map(([id, flow]) => `<option value="${escapeHtml(id)}">${escapeHtml(flow.title)}</option>`)
      .join("");
    flowSelect.value = activeFlowId;
  }

  function renderFlow() {
    const flow = flowDefinitions[activeFlowId] || flowDefinitions.overview;
    const query = searchInput.value.trim().toLowerCase();
    const relation = relatedSet(activeFunction);
    const allNodes = nodesForFlow(flow);
    const treeNodes = collectCallTreeNodes(activeFunction, treeDisplayDepth()).map((item) => item.id);
    const searchableNodes = uniq([...allNodes, ...treeNodes]);
    const matches = query
      ? searchableNodes.filter((id) => nodeSearchText(id).includes(query))
      : [];

    flowTitle.textContent = flow.title;
    flowSummary.textContent = flow.summary;
    flowStats.textContent = `${allNodes.length} 个流程节点 / ${Object.keys(functionCatalog).length} 个目录函数`;
    renderSearchResults(query);

    const flowHtml = flow.stages.map((s) => {
      const maxLaneCount = Math.max(2, Math.min(5, s.lanes.length));
      return [
        '<section class="stage-band">',
        '<div class="stage-header">',
        `<div><h3 class="stage-title">${escapeHtml(s.title)}</h3><p class="stage-desc">${escapeHtml(s.desc)}</p></div>`,
        `<span class="stage-desc">${s.lanes.reduce((acc, l) => acc + l.nodes.length, 0)} nodes</span>`,
        '</div>',
        `<div class="lane-grid" style="grid-template-columns: repeat(${maxLaneCount}, minmax(155px, 1fr));">`,
        s.lanes.map((l) => renderLane(l, flow, relation, query, matches)).join(""),
        '</div>',
        '</section>',
      ].join("");
    }).join("");
    flowGraph.innerHTML = flowHtml + renderSelectedFunctionFlow(query, matches);

    flowGraph.querySelectorAll("[data-node-id]").forEach((button) => {
      button.addEventListener("click", () => {
        navigateToFunction(button.dataset.nodeId);
      });
      button.addEventListener("dblclick", () => {
        navigateToFunction(button.dataset.nodeId, { pushHistory: false });
        focusMode = true;
        renderAll();
      });
    });

    searchCount.textContent = query ? `当前流程匹配 ${matches.length} 个函数；全局匹配 ${searchCatalog(query).length} 个函数` : "";
  }

  function renderLane(l, flow, relation, query, matches) {
    const body = l.nodes.map((id) => renderNode(id, flow, relation, query, matches)).join("");
    return [
      '<div class="lane">',
      `<div class="lane-title"><span>${escapeHtml(l.title)}</span><span>${l.nodes.length}</span></div>`,
      `<div class="node-stack">${body || '<div class="empty-lane">无节点</div>'}</div>`,
      '</div>',
    ].join("");
  }

  function renderNode(id, flow, relation, query, matches) {
    const meta = functionCatalog[id] || fn(id, { purpose: "未定义函数" });
    const hiddenByDepth = !visibleByDepth(flow, id);
    const hiddenByFocus = focusMode && !relation.has(id);
    const searchHit = query && matches.includes(id);
    const classes = [
      "flow-node",
      meta.external ? "is-external" : "",
      id === activeFunction ? "active" : "",
      incomingOf(activeFunction).includes(id) ? "is-caller" : "",
      outgoingOf(activeFunction).includes(id) ? "is-callee" : "",
      hiddenByDepth || hiddenByFocus ? "is-hidden" : "",
      searchHit ? "search-hit" : "",
    ].filter(Boolean).join(" ");
    const calls = outgoingOf(id);
    const callText = calls.length ? `调用: ${calls.slice(0, 4).join(" / ")}${calls.length > 4 ? " ..." : ""}` : "调用: 无直接子调用";
    return [
      `<button type="button" class="${classes}" data-node-id="${escapeHtml(id)}">`,
      `<span class="flow-node-name">${escapeHtml(id)}</span>`,
      `<span class="flow-node-module">${escapeHtml(meta.file || meta.kind)}</span>`,
      `<span class="flow-node-desc">${escapeHtml(meta.role || meta.purpose)}</span>`,
      `<span class="flow-node-calls">${escapeHtml(callText)}</span>`,
      '</button>',
    ].join("");
  }

  function treeDisplayDepth() {
    return depthSelect.value === "all" ? 8 : Number(depthSelect.value);
  }

  function collectCallTreeNodes(root, limit) {
    const rows = [];
    function walk(id, depth, path) {
      if (!functionCatalog[id]) {
        return;
      }
      rows.push({ id, depth, repeated: path.has(id) });
      if (depth >= limit || path.has(id)) {
        return;
      }
      const nextPath = new Set(path);
      nextPath.add(id);
      outgoingOf(id).forEach((child) => walk(child, depth + 1, nextPath));
    }
    walk(root, 0, new Set());
    return rows;
  }

  function renderSelectedFunctionFlow(query, matches) {
    if (!activeFunction || !functionCatalog[activeFunction]) {
      return "";
    }
    const limit = treeDisplayDepth();
    const rows = collectCallTreeNodes(activeFunction, limit);
    const levels = [];
    rows.forEach((row) => {
      if (!levels[row.depth]) {
        levels[row.depth] = [];
      }
      if (!levels[row.depth].some((item) => item.id === row.id && item.repeated === row.repeated)) {
        levels[row.depth].push(row);
      }
    });
    const expandedCount = uniq(rows.map((row) => row.id)).length;
    const depthLabel = depthSelect.value === "all" ? `最多 ${limit} 层` : `${limit} 层`;
    return [
      '<section class="stage-band recursive-flow">',
      '<div class="stage-header">',
      `<div><h3 class="stage-title">选中函数递归调用流程</h3><p class="stage-desc">从 <code>${escapeHtml(activeFunction)}</code> 开始，将函数内部调用的子函数按层级展开；点击任意节点继续递归展开。</p></div>`,
      `<span class="stage-desc">${expandedCount} functions / ${depthLabel}</span>`,
      '</div>',
      '<div class="call-flow-levels">',
      levels.map((items, depth) => renderCallFlowLevel(items || [], depth, query, matches)).join(""),
      '</div>',
      '</section>',
    ].join("");
  }

  function renderCallFlowLevel(items, depth, query, matches) {
    return [
      '<div class="call-flow-level">',
      `<div class="call-flow-level-title">Level ${depth}</div>`,
      '<div class="node-stack">',
      items.length ? items.map((item) => renderCallFlowNode(item, query, matches)).join("") : '<div class="empty-lane">无节点</div>',
      '</div>',
      '</div>',
    ].join("");
  }

  function renderCallFlowNode(item, query, matches) {
    const id = item.id;
    const meta = functionCatalog[id] || fn(id, { purpose: "未定义函数" });
    const calls = outgoingOf(id);
    const classes = [
      "flow-node",
      "tree-flow-node",
      meta.external ? "is-external" : "",
      id === activeFunction ? "active" : "",
      incomingOf(activeFunction).includes(id) ? "is-caller" : "",
      outgoingOf(activeFunction).includes(id) ? "is-callee" : "",
      query && matches.includes(id) ? "search-hit" : "",
      item.repeated ? "is-repeated" : "",
    ].filter(Boolean).join(" ");
    const callText = item.repeated
      ? "循环/重复引用，已停止继续展开"
      : calls.length ? `继续调用: ${calls.slice(0, 5).join(" / ")}${calls.length > 5 ? " ..." : ""}` : "叶子函数: 无直接子调用";
    return [
      `<button type="button" class="${classes}" data-node-id="${escapeHtml(id)}">`,
      `<span class="flow-node-name">${escapeHtml(id)}</span>`,
      `<span class="flow-node-module">${escapeHtml(meta.file || meta.kind)}</span>`,
      `<span class="flow-node-desc">${escapeHtml(meta.role || meta.purpose)}</span>`,
      `<span class="flow-node-calls">${escapeHtml(callText)}</span>`,
      '</button>',
    ].join("");
  }

  function nodeSearchText(id) {
    const meta = functionCatalog[id] || {};
    return [id, meta.file, meta.kind, meta.input, meta.output, meta.purpose, meta.role, meta.source, ...(meta.logicNotes || []), ...(meta.sideEffects || []), ...(meta.calls || [])]
      .join("\n")
      .toLowerCase();
  }

  function searchCatalog(query) {
    if (!query) {
      return [];
    }
    return Object.keys(functionCatalog)
      .filter((id) => nodeSearchText(id).includes(query))
      .sort((left, right) => {
        const leftName = left.toLowerCase().includes(query) ? 0 : 1;
        const rightName = right.toLowerCase().includes(query) ? 0 : 1;
        return leftName - rightName || left.localeCompare(right);
      });
  }

  function renderSearchResults(query) {
    if (!searchResults) {
      return;
    }
    const hits = searchCatalog(query);
    if (!query) {
      searchResults.innerHTML = '<p class="hint">输入函数名、文件名、状态字段或流程关键词后，可在全局函数目录中定位。</p>';
      return;
    }
    if (!hits.length) {
      searchResults.innerHTML = '<p class="hint">全局函数目录没有匹配项。</p>';
      return;
    }
    searchResults.innerHTML = [
      `<div class="search-title">全局匹配 ${hits.length} 个函数</div>`,
      '<div class="search-list">',
      hits.slice(0, 40).map((id) => {
        const meta = functionCatalog[id];
        return [
          `<button type="button" class="search-result" data-search-node="${escapeHtml(id)}">`,
          `<span class="search-result-name">${escapeHtml(id)}</span>`,
          `<span class="search-result-meta">${escapeHtml(meta.file || meta.kind)} | 输入: ${escapeHtml(meta.input)} | 输出: ${escapeHtml(meta.output)}</span>`,
          `<span class="search-result-desc">${escapeHtml(meta.purpose || meta.role || "")}</span>`,
          '</button>',
        ].join("");
      }).join(""),
      hits.length > 40 ? '<p class="hint">只显示前 40 个结果，请继续输入缩小范围。</p>' : "",
      '</div>',
    ].join("");
    searchResults.querySelectorAll("[data-search-node]").forEach((button) => {
      button.addEventListener("click", () => {
        navigateToFunction(button.dataset.searchNode);
      });
    });
  }

  function renderDetail() {
    const meta = functionCatalog[activeFunction];
    if (!meta) {
      detailNavigation.innerHTML = "";
      functionDetail.innerHTML = "<p>请选择一个函数节点。</p>";
      functionLogic.innerHTML = "<p>请选择一个函数节点。</p>";
      return;
    }
    const callers = incomingOf(activeFunction);
    const callees = outgoingOf(activeFunction);
    renderDetailNavigation();
    functionDetail.innerHTML = [
      `<h2><code>${escapeHtml(activeFunction)}</code></h2>`,
      detailRow("文件", meta.file || "未记录"),
      detailRow("类型", meta.kind),
      detailRow("输入", meta.input),
      detailRow("输出/副作用", meta.output),
      detailRow("功能目的", meta.purpose),
      detailRow("在当前流程中的作用", meta.role),
      detailRow("关键副作用", meta.sideEffects && meta.sideEffects.length ? meta.sideEffects.join("；") : "无额外记录"),
      relationTable("上游调用方", callers, "当前目录中没有直接调用方，通常是流程入口或 UVM 外部触发。"),
      relationTable("下游子调用", callees, "无直接子调用。"),
      detailPills("调用方", callers, "当前目录中没有直接调用方，通常是流程入口或 UVM 外部触发。"),
      detailPills("子调用", callees, "无直接子调用。"),
    ].join("");
    functionDetail.querySelectorAll("[data-jump-node]").forEach((button) => {
      button.addEventListener("click", () => {
        navigateToFunction(button.dataset.jumpNode);
      });
    });
    renderFunctionLogic(meta, callees);
  }

  function renderDetailNavigation() {
    const parent = navigationStack[navigationStack.length - 1];
    const trail = navigationStack.concat(activeFunction).slice(-5);
    detailNavigation.innerHTML = [
      '<div class="nav-actions">',
      `<button id="back-to-parent" type="button" ${parent ? "" : "disabled"}>${parent ? `返回母函数：${escapeHtml(parent)}` : "没有可返回的母函数"}</button>`,
      '</div>',
      `<div class="breadcrumb" aria-label="函数浏览路径">${trail.map((id, index) => {
        const isCurrent = index === trail.length - 1;
        return `<span class="${isCurrent ? "current" : ""}">${escapeHtml(id)}</span>`;
      }).join('<span class="separator">/</span>')}</div>`,
    ].join("");
    const backButton = $("#back-to-parent");
    if (backButton) {
      backButton.addEventListener("click", goBackToParent);
    }
  }

  function renderFunctionLogic(meta, callees) {
    const steps = directLogicSteps(activeFunction, meta);
    const pseudo = renderPseudoCode(activeFunction, meta);
    const source = meta.source || pseudo;
    const lines = source.split("\n");
    functionLogic.innerHTML = [
      '<div class="logic-block">',
      '<h2>内部代码逻辑</h2>',
      `<ol class="logic-steps">${steps.map((step) => `<li>${escapeHtml(step)}</li>`).join("")}</ol>`,
      meta.logicNotes && meta.logicNotes.length
        ? `<div class="source-note">${meta.logicNotes.map((note) => `<p>${escapeHtml(note)}</p>`).join("")}</div>`
        : "",
      '</div>',
      '<div class="logic-block">',
      `<h2>${meta.source ? "源码片段" : "近似源码骨架"}</h2>`,
      `<pre class="code-panel"><code>${escapeHtml(source)}</code></pre>`,
      meta.source ? '<p class="detail-value source-note">该源码片段来自当前测试框架源码，用于支撑函数调用关系和逻辑解释；未摘录的局部变量声明或错误处理以源码文件为准。</p>' : "",
      '</div>',
      '<div class="logic-block">',
      '<h2>代码讲解</h2>',
      `<ol class="line-explain">${lines.map((line) => `<li><code>${escapeHtml(line.trim() || " ")}</code><span>${escapeHtml(explainCodeLine(line))}</span></li>`).join("")}</ol>`,
      '</div>',
      '<div class="logic-block">',
      '<h2>继续下钻</h2>',
      callees.length
        ? `<ul class="pill-list">${callees.map((id) => `<li><button type="button" data-logic-jump="${escapeHtml(id)}">${escapeHtml(id)}</button></li>`).join("")}</ul>`
        : '<p class="detail-value">该函数没有记录到子函数，可通过调用树返回上层或切换流程继续浏览。</p>',
      '</div>',
    ].join("");
    functionLogic.querySelectorAll("[data-logic-jump]").forEach((button) => {
      button.addEventListener("click", () => {
        navigateToFunction(button.dataset.logicJump);
      });
    });
  }

  function detailRow(label, value) {
    return [
      '<div class="detail-row">',
      `<div class="detail-label">${escapeHtml(label)}</div>`,
      `<p class="detail-value">${escapeHtml(value)}</p>`,
      '</div>',
    ].join("");
  }

  function detailPills(label, ids, emptyText) {
    const pills = ids.length
      ? `<ul class="pill-list">${ids.map((id) => {
          const externalClass = functionCatalog[id]?.external ? " external-pill" : "";
          return `<li><button class="${externalClass}" type="button" data-jump-node="${escapeHtml(id)}">${escapeHtml(id)}</button></li>`;
        }).join("")}</ul>`
      : `<p class="detail-value">${escapeHtml(emptyText)}</p>`;
    return [
      '<div class="detail-row">',
      `<div class="detail-label">${escapeHtml(label)}</div>`,
      pills,
      '</div>',
    ].join("");
  }

  function relationTable(label, ids, emptyText) {
    if (!ids.length) {
      return [
        '<div class="detail-row">',
        `<div class="detail-label">${escapeHtml(label)}</div>`,
        `<p class="detail-value">${escapeHtml(emptyText)}</p>`,
        '</div>',
      ].join("");
    }
    return [
      '<div class="detail-row">',
      `<div class="detail-label">${escapeHtml(label)}</div>`,
      '<div class="relation-table">',
      ids.map((id) => {
        const item = functionCatalog[id] || fn(id, { purpose: "未定义函数" });
        return [
          `<button type="button" class="relation-row" data-jump-node="${escapeHtml(id)}">`,
          `<span class="relation-name">${escapeHtml(id)}</span>`,
          `<span><strong>输入</strong>${escapeHtml(item.input || "无")}</span>`,
          `<span><strong>输出</strong>${escapeHtml(item.output || "无")}</span>`,
          `<span><strong>功能</strong>${escapeHtml(item.purpose || item.role || "未记录")}</span>`,
          '</button>',
        ].join("");
      }).join(""),
      '</div>',
      '</div>',
    ].join("");
  }

  function renderCallTree() {
    const limit = treeDisplayDepth();
    callChain.innerHTML = renderTree(activeFunction, limit, new Set(), 0);
    callChain.querySelectorAll("[data-tree-node]").forEach((button) => {
      button.addEventListener("click", () => {
        navigateToFunction(button.dataset.treeNode);
      });
    });
  }

  function renderTree(id, limit, seen, depth) {
    if (!functionCatalog[id]) {
      return `<p class="tree-note">未定义函数: ${escapeHtml(id)}</p>`;
    }
    const calls = outgoingOf(id);
    const stop = depth >= limit || !calls.length;
    const note = stop && calls.length ? '<span class="tree-note"> ... 深度限制</span>' : "";
    const children = stop
      ? ""
      : `<ol class="call-tree">${calls.map((child) => {
          if (seen.has(child)) {
            return `<li><button type="button" data-tree-node="${escapeHtml(child)}">${escapeHtml(child)}</button> <span class="tree-note">循环/重复引用</span></li>`;
          }
          const nextSeen = new Set(seen);
          nextSeen.add(child);
          return `<li><button type="button" data-tree-node="${escapeHtml(child)}">${escapeHtml(child)}</button>${renderTree(child, limit, nextSeen, depth + 1)}</li>`;
        }).join("")}</ol>`;
    if (depth === 0) {
      return `<ol class="call-tree"><li><button type="button" data-tree-node="${escapeHtml(id)}">${escapeHtml(id)}</button>${note}${children}</li></ol>`;
    }
    return `${note}${children}`;
  }

  function renderValidation() {
    const errors = validateGraph();
    if (!errors.length) {
      return;
    }
    const warning = document.createElement("section");
    warning.className = "stage-band";
    warning.innerHTML = [
      '<div class="stage-header">',
      '<div><h3 class="stage-title">流程图数据校验告警</h3><p class="stage-desc">以下引用需要补齐，否则说明函数调用关系仍有遗漏。</p></div>',
      '</div>',
      '<div class="lane" style="padding: 12px;">',
      `<ul>${errors.map((e) => `<li>${escapeHtml(e)}</li>`).join("")}</ul>`,
      '</div>',
    ].join("");
    flowGraph.prepend(warning);
  }

  function renderAll() {
    focusButton.textContent = focusMode ? "取消聚焦" : "聚焦所选";
    document.body.classList.toggle("mobile-preview-mode", mobilePreviewMode);
    mobilePreviewToggle.setAttribute("aria-pressed", mobilePreviewMode ? "true" : "false");
    mobilePreviewToggle.textContent = mobilePreviewMode ? "退出手机预览" : "手机预览";
    renderFlowOptions();
    renderFlow();
    renderValidation();
    renderDetail();
    renderCallTree();
  }

  flowSelect.addEventListener("change", () => {
    activeFlowId = flowSelect.value;
    const nodes = nodesForFlow(flowDefinitions[activeFlowId]);
    activeFunction = nodes[0] || activeFunction;
    focusMode = false;
    navigationStack.length = 0;
    renderAll();
  });

  depthSelect.addEventListener("change", renderAll);
  searchInput.addEventListener("input", () => {
    focusMode = false;
    renderAll();
  });
  focusButton.addEventListener("click", () => {
    focusMode = !focusMode;
    focusButton.textContent = focusMode ? "取消聚焦" : "聚焦所选";
    renderAll();
  });
  resetButton.addEventListener("click", () => {
    searchInput.value = "";
    depthSelect.value = "all";
    focusMode = false;
    focusButton.textContent = "聚焦所选";
    renderAll();
  });
  mobilePreviewToggle.addEventListener("click", () => {
    mobilePreviewMode = !mobilePreviewMode;
    renderAll();
  });

  renderAll();
})();
