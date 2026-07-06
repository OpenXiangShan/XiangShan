(function () {
  "use strict";

  const $ = (selector) => document.querySelector(selector);

  const flowSelect = $("#flow-select");
  const depthSelect = $("#depth-select");
  const searchInput = $("#search-input");
  const focusButton = $("#focus-neighborhood");
  const resetButton = $("#reset-view");
  const searchCount = $("#search-count");
  const flowSummary = $("#flow-summary");
  const flowTitle = $("#flow-title");
  const flowStats = $("#flow-stats");
  const flowGraph = $("#flow-graph");
  const functionDetail = $("#function-detail");
  const callChain = $("#call-chain");

  let activeFlowId = "overview";
  let activeFunction = "tc_dispatch_real_smoke::main_phase";
  let focusMode = false;

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

    "memblock_lsqenq_dispatch_base_sequence::body": fn("memblock_lsqenq_dispatch_base_sequence::body", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv",
      input: "default sequence 启动",
      output: "持续驱动 LSQ admission",
      purpose: "把主表中的 LS transaction 送入 DUT enqLsq。",
      role: "默认开启真实 admission；入队成功后激活 uid、构建 TLB、路由 issue queue。",
      calls: ["seq_csr_common::init", "configure_lsqenq_from_plus", "lsqenq_sequence::ensure_helpers", "wait_for_main_table", "drive_lsqenq_loop"],
    }),
    "configure_lsqenq_from_plus": fn("configure_lsqenq_from_plus", { file: "memblock_lsqenq_dispatch_base_sequence.sv", input: "seq_csr_common getter", output: "enq 参数", purpose: "读取 admission 相关参数。", role: "读取 enable、idle_stop、ready_timeout、start_timeout；LSQENQ 不再读取专用 max_cycles。" }),
    "lsqenq_sequence::ensure_helpers": fn("lsqenq_sequence::ensure_helpers", {
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
      calls: ["admit_non_lsq_if_ready", "collect_lsq_candidates", "clear_lsqenq_xaction", "assign_lsqenq_slot", "lsqenq_agent_driver::send_pkt", "confirm_lsq_candidates"],
    }),
    "admit_non_lsq_if_ready": fn("admit_non_lsq_if_ready", { file: "memblock_lsqenq_dispatch_base_sequence.sv", input: "next uid", output: "非 LSQ uid 可能 active/route", purpose: "处理 need_alloc=0 的简化流。", role: "不需要 DUT enqLsq 接口时仍然要激活状态并进入后续 route。" }),
    "collect_lsq_candidates": fn("collect_lsq_candidates", {
      file: "memblock_lsqenq_dispatch_base_sequence.sv",
      input: "data.get_next_new_admit_uid()、free count、DUT 可接收约束",
      output: "本拍候选 uid/tr/behavior/lq/sq key",
      purpose: "选择本拍能入队的连续 uid。",
      role: "不再维护本地 next_admit_uid；从 common_data 公共入队进度取起点，同时考虑 get_enq_per_cycle() 固定/随机上限、本地 LSQ free count、flush 阻塞和队列元素数。",
      calls: ["admission_blocked_by_flush", "next_uid_needs_lsq_admission", "lsq_ctrl_model::can_allocate", "lsq_ctrl_model::preview_allocate"],
    }),
    "admission_blocked_by_flush": fn("admission_blocked_by_flush", { file: "memblock_lsqenq_dispatch_base_sequence.sv", input: "common_data flush 状态", output: "bit", purpose: "判断 admission 是否被全局 flush 阻塞。", role: "redirect/flush 期间不允许新 uid 入队。" }),
    "next_uid_needs_lsq_admission": fn("next_uid_needs_lsq_admission", { file: "memblock_lsqenq_dispatch_base_sequence.sv", input: "data.get_next_new_admit_uid()", output: "uid/main_tr/behavior", purpose: "判断下一个 uid 是否需要 LSQ admission。", role: "从 common_data 的 max_enqueued_uid 推导下一条 uid，维护 admission 顺序，并跳过不需要 LSQ 分配或已处理项。" }),
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
      calls: ["lsqenq_sequence::drain_csr_runtime_events", "common_data_transaction::set_status_field", "issue_queue_scheduler::route_uid"],
    }),
    "common_data_transaction::activate_uid_by_behavior": fn("common_data_transaction::activate_uid_by_behavior", { file: "common_data_transaction.sv", input: "uid, behavior", output: "active ROB/LQ/SQ map", purpose: "根据操作行为激活 uid。", role: "建立 ROB/LQ/SQ 到 uid 的反查映射，用于 monitor/commit/deq 定位。" }),
    "lsqenq_sequence::drain_csr_runtime_events": fn("lsqenq_sequence::drain_csr_runtime_events", { file: "memblock_lsqenq_dispatch_base_sequence.sv", input: "latest_raw_csr", output: "runtime CSR 镜像更新", purpose: "LSQ admission 后立即同步 CSR runtime snapshot。", role: "保证随后 TLB 表构建使用最新 ASID/VMID/s2xlate。" }),
    "tlb_map_builder::build_tlb_entry_for_req": fn("tlb_map_builder::build_tlb_entry_for_req", { file: "tlb_map_builder.sv", input: "vpn, s2xlate, runtime CSR", output: "memblock_tlb_entry", purpose: "L2TLB req miss 时构造 by-key TLB entry。", role: "builder 不持有 common_data，只负责生成 entry 内容。", calls: ["choose_paddr", "randomize_pte_bits", "memblock_tlb_entry::apply_csr_state"] }),
    "build_tlb_table_for_active_uid": fn("build_tlb_table_for_active_uid", {
      file: "memblock_dispatch_base_sequence.sv",
      input: "uid, runtime CSR",
      output: "TLB_MAPPED 状态和 issue queue 路由",
      purpose: "确认 uid active/enq 后允许进入 issue 路径。",
      role: "不再提前建 per-uid TLB 表；真实 entry 由 L2TLB request 触发创建。",
      calls: ["collect_csr_runtime_events", "common_data_transaction::set_status_field", "issue_queue_scheduler::route_uid"],
    }),
    "common_data_transaction::get_or_create_tlb_entry_by_req": fn("common_data_transaction::get_or_create_tlb_entry_by_req", { file: "common_data_transaction.sv", input: "vpn, s2xlate", output: "key, entry, created", purpose: "按 by-key cache 查/建 TLB entry。", role: "同一 {vpn,asid,vmid,s2xlate} 复用同一 entry。" }),
    "common_data_transaction::update_uid_tlb_records_by_entry": fn("common_data_transaction::update_uid_tlb_records_by_entry", { file: "common_data_transaction.sv", input: "key, entry", output: "uid_tlb_record PTE 回填", purpose: "把 L2TLB response 的 PTE 信息回填到匹配 uid record。", role: "用于 debug、追溯和 PTW wait replay ready 判断。" }),
    "memblock_l2tlb_base_sequence::body": fn("memblock_l2tlb_base_sequence::body", {
      file: "mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv",
      input: "default sequence 启动",
      output: "响应 DTLB->L2TLB request",
      purpose: "代替 L2TLB 对上游 DTLB 的 responder 功能。",
      role: "采 request 中的 vpn/s2xlate，结合 runtime CSR 查 common_data TLB 表并回填 response。",
      calls: ["seq_csr_common::init", "configure_l2tlb_from_plus", "wait_for_main_table", "drive_l2tlb_loop"],
    }),
    "configure_l2tlb_from_plus": fn("configure_l2tlb_from_plus", { file: "memblock_l2tlb_base_sequence.sv", input: "plus getter", output: "latency/idle stop cycle", purpose: "读取 L2TLB responder 参数。", role: "控制 response latency 和连续 idle 退出阈值。" }),
    "drive_l2tlb_loop": fn("drive_l2tlb_loop", { file: "memblock_l2tlb_base_sequence.sv", input: "无", output: "持续处理 request", purpose: "L2TLB responder 主循环。", role: "每拍采样 request，按 latency 查表并驱动 response；有 progress 清 idle_count，连续 idle 超过 idle_stop_cycle 后退出。", calls: ["send_l2tlb_cycle"] }),
    "send_l2tlb_cycle": fn("send_l2tlb_cycle", {
      file: "memblock_l2tlb_base_sequence.sv",
      input: "idle_count",
      output: "response xaction 或 idle",
      purpose: "处理一拍 L2TLB request/response。",
      role: "request valid 时采 vpn/s2xlate，查表命中则回填 TLB 表项，并向上层返回 has_progress 以清零 idle_count。",
      calls: ["sample_request_fields", "common_data_transaction::get_or_create_tlb_entry_by_req", "fill_dtlb_resp_from_entry", "common_data_transaction::update_uid_tlb_records_by_entry"],
    }),
    "sample_request_fields": fn("sample_request_fields", { file: "memblock_l2tlb_base_sequence.sv", input: "L2TLB interface", output: "vpn, s2xlate", purpose: "采 DTLB->L2TLB 请求关键字段。", role: "注意该 agent 代替 L2TLB，不是 L2TLB 到 L2Cache/PTW 下游模型。" }),
    "common_data_transaction::get_or_create_tlb_entry_by_req": fn("common_data_transaction::get_or_create_tlb_entry_by_req", { file: "common_data_transaction.sv", input: "vpn, s2xlate, runtime CSR", output: "key, entry, created", purpose: "按 by-key TLB cache 查/建 entry。", role: "key 包含 vpn/asid/vmid/s2xlate；miss 自动建 entry，不再进入缺项策略分支。" }),
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
      calls: ["memblock_sync_pkg::tick_dispatch_service_cycle", "collect_runtime_context_events", "collect_writeback_events", "collect_exception_and_redirect_events", "exception_redirect_replay_task"],
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
    "csr_runtime_monitor::mon_data": fn("csr_runtime_monitor::mon_data", {
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
    "collect_monitor_event_batch": fn("collect_monitor_event_batch", { file: "memblock_dispatch_base_sequence.sv", input: "raw int_wb / iq_feedback / ctrl", output: "batch handler 收到同批 semantic events", purpose: "真实 monitor service 统一入口。", role: "把同一 service cycle 的 writeback、IQ feedback 和 memoryViolation 合并后做 redirect-first 仲裁。", calls: ["dispatch_monitor_event_adapter::collect_writeback_events_batch", "dispatch_monitor_event_adapter::collect_ctrl_redirect_events_batch", "dispatch_monitor_batch_handler::process_monitor_event_batch"] }),
    "dispatch_monitor_event_adapter::collect_writeback_events_batch": fn("dispatch_monitor_event_adapter::collect_writeback_events_batch", { file: "dispatch_monitor_event_adapter.sv", input: "raw_int_wb_q/raw_iq_feedback_q", output: "events[$]", purpose: "转换 writeback/feedback raw event 并放入 batch。", role: "int_wb 主要处理 load wb，IQ feedback 处理 STA/STD pass/replay。", calls: ["convert_raw_int_wb", "convert_raw_iq_feedback"] }),
    "convert_raw_int_wb": fn("convert_raw_int_wb", { file: "dispatch_monitor_event_adapter.sv", input: "dispatch_raw_int_wb_t", output: "memblock_wb_event_t", purpose: "转换 int writeback raw。", role: "用 ROB/LQ/SQ key 和 exception_vec 构造 load writeback/fault/redirect event。", calls: ["event_has_active_uid"] }),
    "convert_raw_iq_feedback": fn("convert_raw_iq_feedback", { file: "dispatch_monitor_event_adapter.sv", input: "dispatch_raw_iq_feedback_t", output: "memblock_wb_event_t", purpose: "转换 STA/STD IQ feedback。", role: "hit/pass 或 backend replay 都从这里进入统一 writeback event。" }),
    "event_has_active_uid": fn("event_has_active_uid", { file: "dispatch_monitor_event_adapter.sv", input: "wb_event key", output: "bit", purpose: "判断 raw event 是否能反查到 active uid。", role: "过滤被 flush 后残留的旧 monitor 事件。" }),
    "dispatch_monitor_batch_handler::process_monitor_event_batch": fn("dispatch_monitor_batch_handler::process_monitor_event_batch", { file: "dispatch_monitor_batch_handler.sv", input: "events[$]", output: "放行事件进入 writeback handler 或 feedback_event_q", purpose: "同批事件仲裁。", role: "先 normalize，再选择 oldest redirect；被 redirect 覆盖的 pass/replay/fault 不落状态。", calls: ["common_data_transaction::normalize_feedback_event", "writeback_status_handler::handle_event", "common_data_transaction::push_feedback_event"] }),
    "writeback_status_handler::handle_event": fn("writeback_status_handler::handle_event", {
      file: "writeback_status_handler.sv",
      input: "memblock_wb_event_t",
      output: "normal pass 状态或 feedback_event_q",
      purpose: "接收 batch handler 已放行的非 redirect 事件。",
      role: "normal pass/fault/IQ feedback 在这里落状态；redirect 不允许绕过 batch handler 进入该入口。",
      calls: ["common_data_transaction::mark_target_normal_pass", "common_data_transaction::push_feedback_event"],
    }),
    "common_data_transaction::normalize_feedback_event": fn("common_data_transaction::normalize_feedback_event", { file: "common_data_transaction.sv", input: "wb_event", output: "补齐 uid/target/epoch", purpose: "规范化反馈事件。", role: "将 ROB/LQ/SQ key 反查 uid，并统一 replay/redirect/fault 判断。" }),
    "common_data_transaction::mark_target_normal_pass": fn("common_data_transaction::mark_target_normal_pass", { file: "common_data_transaction.sv", input: "uid,target,issue_epoch,replay_seq", output: "target pass/writeback 状态", purpose: "标记目标通路正常完成。", role: "会校验 issue_epoch/replay_seq，防止旧反馈污染新发射。", calls: ["conditional_set_target_status_field", "required_targets_done"] }),
    "conditional_set_target_status_field": fn("conditional_set_target_status_field", { file: "common_data_transaction.sv", input: "uid,target,field,epoch", output: "状态位可能更新", purpose: "带版本检查地设置 target 状态。", role: "统一处理旧 issue/replay 反馈过滤。" }),
    "required_targets_done": fn("required_targets_done", { file: "common_data_transaction.sv", input: "uid status", output: "bit", purpose: "判断该 uid 所需 target 是否都完成。", role: "决定 uid 是否可以进入 commit/success 后续条件。" }),
    "common_data_transaction::push_feedback_event": fn("common_data_transaction::push_feedback_event", { file: "common_data_transaction.sv", input: "complex wb_event", output: "feedback_event_q.push_back", purpose: "保存 replay/redirect/fault 复杂事件。", role: "让 exception_redirect_replay_handler 统一按优先级处理复杂恢复。" }),
    "dispatch_monitor_event_adapter::collect_ctrl_redirect_events_batch": fn("dispatch_monitor_event_adapter::collect_ctrl_redirect_events_batch", { file: "dispatch_monitor_event_adapter.sv", input: "raw_ctrl_q", output: "commit deq 或 redirect event batch", purpose: "处理 ctrl raw event。", role: "LQ/SQ deq 直接交 commit handler，memoryViolation 转 redirect event 放入 batch。", calls: ["dispatch_monitor_event_adapter::apply_raw_ctrl_deq", "convert_raw_memory_violation"] }),
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
    "issue_field_assigner::assign_issue_item_fields": fn("issue_field_assigner::assign_issue_item_fields", { file: "issue_field_assigner.sv", input: "tr,item,pipe_idx", output: "完整 issue payload", purpose: "发射字段总入口。", role: "先写主表字段，再补第二/第三类字段和后端 meta。", calls: ["assign_main_issue_fields", "assign_issue_dep_fields", "assign_backend_meta_fields"] }),
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
      role: "优先服务 PTW wait replay，再推进 active redirect；新事件里 redirect 优先级高于 replay/fault。",
      calls: ["service_ptw_wait_replay", "advance_active_redirect", "select_oldest_redirect", "handle_replay_event", "handle_fault_event"],
    }),
    "service_ptw_wait_replay": fn("service_ptw_wait_replay", { file: "exception_redirect_replay_handler.sv", input: "ptw_wait_replay_q", output: "ready replay 重新 pending", purpose: "释放等待 TLB response 的 replay。", role: "PTW-back replay 需要等 L2TLB response done 后再重新入队。", calls: ["common_data_transaction::pop_ready_ptw_wait_replay", "common_data_transaction::mark_replay_pending"] }),
    "advance_active_redirect": fn("advance_active_redirect", { file: "exception_redirect_replay_handler.sv", input: "active redirect state", output: "flush 可能完成", purpose: "推进已启动 redirect 的 recovery。", role: "等待 redirect drive done 后应用 flush，并解除 flush_in_progress。", calls: ["common_data_transaction::redirect_drive_done_for", "common_data_transaction::apply_redirect_flush"] }),
    "select_oldest_redirect": fn("select_oldest_redirect", { file: "exception_redirect_replay_handler.sv", input: "redirect events[$]", output: "最老 redirect", purpose: "多个 redirect 中选择 ROB 最老项。", role: "避免较新的 redirect 先 flush 导致旧异常处理错序。", calls: ["redirect_event_is_older"] }),
    "redirect_event_is_older": fn("redirect_event_is_older", { file: "exception_redirect_replay_handler.sv", input: "candidate,current", output: "bit", purpose: "比较 redirect ROB 年龄。", role: "基于 rob_order_util，不能用普通 int 大小比较。" }),
    "common_data_transaction::request_redirect_flush": fn("common_data_transaction::request_redirect_flush", { file: "common_data_transaction.sv", input: "redirect payload", output: "flush_in_progress=1, flush_epoch++", purpose: "启动 redirect recovery。", role: "冻结 admission/issue/commit，并把 redirect payload 交给 redirect sequence 驱动。", calls: ["common_data_transaction::push_redirect_drive"] }),
    "common_data_transaction::push_redirect_drive": fn("common_data_transaction::push_redirect_drive", { file: "common_data_transaction.sv", input: "redirect payload", output: "redirect_drive_q.push_back", purpose: "排队等待 redirect driver 发送。", role: "让 exception handler 与 redirect_agent sequence 解耦。" }),
    "memblock_redirect_dispatch_base_sequence::body": fn("memblock_redirect_dispatch_base_sequence::body", { file: "memblock_redirect_dispatch_base_sequence.sv", input: "default sequence 启动", output: "持续驱动 redirect 或 idle", purpose: "redirect_agent 的真实驱动 sequence。", role: "从 common_data 取 pending redirect payload 并驱动到 DUT。", calls: ["common_data_transaction::try_pop_redirect_drive", "drive_redirect_payload", "drive_idle_once"] }),
    "common_data_transaction::try_pop_redirect_drive": fn("common_data_transaction::try_pop_redirect_drive", { file: "common_data_transaction.sv", input: "redirect_drive_q", output: "payload", purpose: "取一个待驱动 redirect。", role: "redirect sequence 的输入来源。" }),
    "drive_redirect_payload": fn("drive_redirect_payload", { file: "memblock_redirect_dispatch_base_sequence.sv", input: "redirect payload", output: "redirect_agent xaction", purpose: "真实驱动 redirect 接口。", role: "驱动完成后通知 common_data，exception handler 下一轮才能 apply flush。", calls: ["assign_redirect_xaction", "redirect_agent_driver::send_pkt", "common_data_transaction::mark_redirect_drive_done"] }),
    "assign_redirect_xaction": fn("assign_redirect_xaction", { file: "memblock_redirect_dispatch_base_sequence.sv", input: "payload", output: "redirect xaction fields", purpose: "把软件 redirect payload 转成 agent transaction。", role: "填 robIdx、level、cfiUpdate 等 DUT 接口字段。" }),
    "redirect_agent_driver::send_pkt": fn("redirect_agent_driver::send_pkt", { file: "mem_ut/ver/ut/memblock/agent/redirect_agent_agent/src/redirect_agent_agent_driver.sv", input: "redirect xaction", output: "DUT redirect interface fire", purpose: "真实驱动 redirect 接口。", role: "形成 DUT 侧 flush/recovery 触发。" }),
    "common_data_transaction::mark_redirect_drive_done": fn("common_data_transaction::mark_redirect_drive_done", { file: "common_data_transaction.sv", input: "payload", output: "redirect drive done 标记", purpose: "通知 recovery 可进入 apply flush。", role: "避免同一 delta 内边发射边清状态造成竞争。" }),
    "common_data_transaction::redirect_drive_done_for": fn("common_data_transaction::redirect_drive_done_for", { file: "common_data_transaction.sv", input: "payload", output: "bit", purpose: "查询 redirect 是否已驱动。", role: "exception handler 用它判断是否能应用 flush。" }),
    "common_data_transaction::apply_redirect_flush": fn("common_data_transaction::apply_redirect_flush", { file: "common_data_transaction.sv", input: "redirect payload", output: "被 flush uid retire/clear", purpose: "应用 redirect flush。", role: "按 ROB 顺序清理 younger uid、issue queue、PTW wait replay，并解除全局 flush。", calls: ["common_data_transaction::clear_uid_dispatch_result", "common_data_transaction::retire_active_uid", "common_data_transaction::clear_ptw_wait_replay_by_redirect"] }),
    "common_data_transaction::clear_uid_dispatch_result": fn("common_data_transaction::clear_uid_dispatch_result", { file: "common_data_transaction.sv", input: "uid", output: "dispatch/writeback/pass/fault 状态清理", purpose: "清掉被 flush uid 的发射结果。", role: "避免 redirect 后旧结果继续参与 success/commit 判断。" }),
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
          lane("admission", ["memblock_lsqenq_dispatch_base_sequence::body", "send_lsqenq_cycle", "complete_admission"]),
          lane("issue", ["memblock_issue_dispatch_base_sequence::body", "send_issue_cycle", "issue_queue_scheduler::mark_issue_fire", "submit_issue_accept_pass"]),
          lane("commit", ["memblock_lsqcommit_dispatch_base_sequence::body", "send_lsqcommit_cycle", "lsq_commit_handler::apply_raw_ctrl_deq"]),
          lane("l2tlb", ["memblock_l2tlb_base_sequence::body", "send_l2tlb_cycle", "common_data_transaction::get_or_create_tlb_entry_by_req"]),
          lane("recovery", ["exception_redirect_replay_handler::process_pending_events", "memblock_redirect_dispatch_base_sequence::body", "common_data_transaction::apply_redirect_flush"]),
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
          lane("setup", ["memblock_lsqenq_dispatch_base_sequence::body", "configure_lsqenq_from_plus", "wait_for_main_table", "drive_lsqenq_loop"]),
        ]),
        stage("一拍 admission", "选候选、填 slot、驱动 DUT、确认响应。", [
          lane("cycle", ["send_lsqenq_cycle", "admit_non_lsq_if_ready"]),
          lane("select", ["collect_lsq_candidates", "lsq_ctrl_model::can_allocate", "lsq_ctrl_model::preview_allocate"]),
          lane("drive", ["assign_lsqenq_slot", "lsqenq_agent_driver::send_pkt", "lsqenq_agent_driver::wait_lsq_can_accept", "lsqenq_agent_driver::sample_lsqenq_resp"]),
          lane("commit", ["confirm_lsq_candidates", "lsq_ctrl_model::commit_allocate_with_resp", "complete_admission"]),
        ]),
        stage("入队后处理", "active/enq 在 commit_allocate_with_resp 内完成；complete_admission 继续做 CSR drain、TLB builder 和 issue route。", [
          lane("active/enq", ["lsq_ctrl_model::commit_allocate_with_resp", "common_data_transaction::set_main_transaction", "common_data_transaction::activate_uid_by_behavior"]),
          lane("tlb", ["complete_admission", "common_data_transaction::set_status_field", "issue_queue_scheduler::route_uid"]),
          lane("route", ["issue_queue_scheduler::route_uid"]),
        ]),
      ],
    },
    l2tlb: {
      title: "TLB 表与 L2TLB responder",
      summary: "L2TLB agent 代替的是 L2TLB 对上游 DTLB 的 responder；查表 key 来自 request vpn/s2xlate 和 runtime CSR 的 ASID/VMID。",
      stages: [
        stage("TLB 表生成", "L2TLB request 到来时按 by-key cache 查/建 TLB entry；uid 发射后只登记 TLB 上下文记录。", [
          lane("builder", ["common_data_transaction::get_or_create_tlb_entry_by_req", "common_data_transaction::build_tlb_entry_for_key", "tlb_map_builder::build_tlb_entry_for_req"]),
        ]),
        stage("responder sequence", "采 request，按 by-key entry 回包，并回填匹配 uid record。", [
          lane("setup", ["memblock_l2tlb_base_sequence::body", "configure_l2tlb_from_plus", "wait_for_main_table", "drive_l2tlb_loop"]),
          lane("request", ["send_l2tlb_cycle", "sample_request_fields"]),
          lane("lookup", ["common_data_transaction::get_or_create_tlb_entry_by_req", "common_data_transaction::build_tlb_entry_for_key"]),
          lane("response", ["fill_dtlb_resp_from_entry", "common_data_transaction::update_uid_tlb_records_by_entry"]),
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
          lane("csr", ["csr_runtime_monitor::mon_data", "memblock_sync_pkg::push_raw_csr"]),
          lane("sfence", ["fence_agent_agent_monitor::mon_data", "memblock_sync_pkg::push_raw_sfence"]),
        ]),
        stage("writeback raw", "int_wb、IQ feedback 和 ctrl memoryViolation 转换为同批 memblock_wb_event_t。", [
          lane("collect", ["collect_monitor_event_batch", "dispatch_monitor_event_adapter::collect_writeback_events_batch", "dispatch_monitor_event_adapter::collect_ctrl_redirect_events_batch"]),
          lane("convert", ["convert_raw_int_wb", "convert_raw_iq_feedback", "event_has_active_uid"]),
          lane("batch arbiter", ["dispatch_monitor_batch_handler::process_monitor_event_batch", "writeback_status_handler::handle_event"]),
        ]),
        stage("状态更新", "普通 pass 直接写状态，复杂事件入队。", [
          lane("normal pass", ["writeback_status_handler::handle_event", "common_data_transaction::normalize_feedback_event", "common_data_transaction::mark_target_normal_pass", "conditional_set_target_status_field", "required_targets_done"]),
          lane("complex", ["common_data_transaction::push_feedback_event"]),
        ]),
        stage("ctrl/redirect raw", "ctrl monitor 的 deq 直接同步，memoryViolation 进入同批 redirect 仲裁。", [
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
          lane("select", ["select_oldest_redirect", "redirect_event_is_older"]),
          lane("freeze", ["common_data_transaction::request_redirect_flush", "common_data_transaction::push_redirect_drive"]),
          lane("drive", ["memblock_redirect_dispatch_base_sequence::body", "common_data_transaction::try_pop_redirect_drive", "drive_redirect_payload", "assign_redirect_xaction", "redirect_agent_driver::send_pkt", "common_data_transaction::mark_redirect_drive_done"]),
          lane("apply", ["advance_active_redirect", "common_data_transaction::redirect_drive_done_for", "common_data_transaction::apply_redirect_flush", "common_data_transaction::clear_uid_dispatch_result", "common_data_transaction::clear_ptw_wait_replay_by_redirect"]),
        ]),
        stage("backend replay", "replay 清旧队列、bump 版本，再让 route_uid 重新入队。", [
          lane("handle", ["handle_replay_event", "common_data_transaction::resolve_uid_for_event", "common_data_transaction::push_ptw_wait_replay"]),
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
          lane("active", ["common_data_transaction::activate_uid_by_behavior", "common_data_transaction::mark_issue_snapshot", "common_data_transaction::mark_target_normal_pass"]),
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
        activeFunction = button.dataset.nodeId;
        focusMode = false;
        renderAll();
      });
      button.addEventListener("dblclick", () => {
        activeFunction = button.dataset.nodeId;
        focusMode = true;
        renderAll();
      });
    });

    searchCount.textContent = query ? `当前流程匹配 ${matches.length} 个函数` : "";
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
    return [id, meta.file, meta.kind, meta.purpose, meta.role, ...(meta.sideEffects || []), ...(meta.calls || [])]
      .join("\n")
      .toLowerCase();
  }

  function renderDetail() {
    const meta = functionCatalog[activeFunction];
    if (!meta) {
      functionDetail.innerHTML = "<p>请选择一个函数节点。</p>";
      return;
    }
    const callers = incomingOf(activeFunction);
    const callees = outgoingOf(activeFunction);
    functionDetail.innerHTML = [
      `<h2><code>${escapeHtml(activeFunction)}</code></h2>`,
      detailRow("文件", meta.file || "未记录"),
      detailRow("类型", meta.kind),
      detailRow("输入", meta.input),
      detailRow("输出/副作用", meta.output),
      detailRow("功能目的", meta.purpose),
      detailRow("在当前流程中的作用", meta.role),
      detailRow("关键副作用", meta.sideEffects && meta.sideEffects.length ? meta.sideEffects.join("；") : "无额外记录"),
      detailPills("调用方", callers, "当前目录中没有直接调用方，通常是流程入口或 UVM 外部触发。"),
      detailPills("子调用", callees, "无直接子调用。"),
    ].join("");
    functionDetail.querySelectorAll("[data-jump-node]").forEach((button) => {
      button.addEventListener("click", () => {
        activeFunction = button.dataset.jumpNode;
        focusMode = false;
        renderAll();
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

  function renderCallTree() {
    const limit = treeDisplayDepth();
    callChain.innerHTML = renderTree(activeFunction, limit, new Set(), 0);
    callChain.querySelectorAll("[data-tree-node]").forEach((button) => {
      button.addEventListener("click", () => {
        activeFunction = button.dataset.treeNode;
        focusMode = false;
        renderAll();
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

  renderAll();
})();
