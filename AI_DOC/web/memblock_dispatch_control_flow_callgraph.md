# MemBlock Dispatch 测试框架控制流与函数调用关系

本文基于当前源码整理，目标是把 mem_ut 中 dispatch 测试框架按“程序流框图”方式展开，覆盖激励生成、LSQ admission、TLB/L2TLB、issue queue、调度发射、monitor 采集、writeback/commit/deq、状态管理、异常 redirect、backend replay。本文只描述当前源码已经存在的控制流和 helper API，便于后续直接按调用链实现或排查问题。

关键源码入口：

- `mem_ut/ver/ut/memblock/tc/src/tc_base.sv`
- `mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_smoke.sv`
- `mem_ut/ver/ut/memblock/env/src/memblock_env.sv`
- `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/*.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_*dispatch*.sv`

## 1. 总体控制流总图

真实 DUT flow 不是一个单独 sequence 串行调用所有逻辑，而是 UVM main phase 下多个 agent default sequence 并行运行，共同读写 `common_data_transaction`。

```text
tc_dispatch_real_smoke::main_phase
  -> memblock_sync_pkg::dispatch_real_smoke_active = 1
  -> super.main_phase()
  -> run_real_smoke_sequence()
       -> memblock_main_dispatch_auto_build_main_table_base_sequence::start(null)

tc_base::build_phase
  -> 创建 memblock_env
  -> 给 agent sqr.main_phase 绑定默认 sequence
       lsqenq    -> memblock_lsqenq_dispatch_base_sequence
       lintsissue -> memblock_issue_dispatch_base_sequence
       lsqcommit -> memblock_lsqcommit_dispatch_base_sequence
       redirect  -> memblock_redirect_dispatch_base_sequence
       L2TLB     -> memblock_l2tlb_base_sequence

memblock_env::build_phase
  -> 创建所有 agent、fifo、rm、scb
memblock_env::connect_phase
  -> 连接 monitor analysis_port 到 RM FIFO

memblock_main_dispatch_auto_build_main_table_base_sequence::body
  -> build_main_table()
  -> service_real_dispatch_flow()
       loop:
         service_monitor_once()
           -> tick_dispatch_service_cycle()
           -> collect_csr_runtime_events()
           -> collect_writeback_events()
           -> collect_exception_and_redirect_events()
           -> exception_redirect_replay_task()
         route_all_issue_queues()
         all_transactions_terminal_done() ? end : #1
  -> data.end_test_check()

并行 agent sequence：
  memblock_lsqenq_dispatch_base_sequence
    -> 等 main_table_ready
    -> 驱动 enqLsq
    -> 确认 DUT resp
    -> activate uid + build TLB + route issue queue

  memblock_issue_dispatch_base_sequence
    -> 等 main_table_ready
    -> route_all_ready_uids()
    -> select_issue_candidates()
    -> assign_issue_item_fields()
    -> 驱动 intIssue 0..6
    -> mark_issue_fire()

  memblock_lsqcommit_dispatch_base_sequence
    -> 等 main_table_ready
    -> select_rob_commit_batch()
    -> 驱动 pendingPtr/flushSb
    -> mark_rob_commit_batch()

  memblock_redirect_dispatch_base_sequence
    -> 常驻 idle
    -> data.try_pop_redirect_drive()
    -> 驱动 redirect_agent
    -> data.mark_redirect_drive_done()

  memblock_l2tlb_base_sequence
    -> 采 DTLB->L2TLB request vpn/s2xlate
    -> data.get_or_create_tlb_entry_by_req()
    -> fill_dtlb_resp_from_entry()
    -> data.update_uid_tlb_records_by_entry()
```

核心原则：

- `common_data_transaction` 是唯一公共数据 owner，保存主表、状态表、by-key TLB 表、uid TLB record、issue queue、raw feedback event、redirect drive queue、active ROB/LQ/SQ 索引。
- `memblock_sync_pkg` 是 monitor/driver/sequence 之间的全局同步层，保存 raw monitor queue、flush epoch、service cycle、capture enable、L2TLB takeover 标志。
- monitor 不直接改 per-uid 状态，而是先把事实写入 `memblock_sync_pkg` raw queue；`dispatch_monitor_event_adapter` 再把 raw event 转成 `memblock_wb_event_t` 或 LSQ deq 更新。
- 发射队列保存还没成功发射的 item；`mark_issue_fire()` 成功后立刻删除队列项，retire 时 `remove_uid_from_issue_queues()` 只是兜底清理。
- replay 分 backend replay 和 MemBlock 内部 load replay。本文中的 replay 指 backend replay：从 writeback/IQ feedback 事件进入 `replay_pending`，清旧队列，更新 `replay_seq`，重新 route 到 LOAD/STA/STD 队列。

## 2. 阶段 A：初始化与参数加载

### A.1 testcase/env 控制流

```text
tc_dispatch_real_smoke::build_phase(phase)
  输入: uvm_phase
  输出/副作用:
    - seq_csr_common 从 plus 重新加载参数
    - 创建 real_smoke_cfg 并放入 uvm_config_db
    - 调 super.build_phase 创建 env
    - 覆盖非 dispatch 关键 agent default sequence 为 idle/default
  调用:
    seq_csr_common::reload_from_plus()
    configure_real_smoke_env_cfg(real_smoke_cfg)
    uvm_config_db::set(this, "env", "cfg", real_smoke_cfg)
    super.build_phase(phase)
    configure_real_smoke_default_sequences()

tc_base::build_phase(phase)
  输入: uvm_phase
  输出/副作用:
    - 从 config_db 获取 tc_if vif
    - 创建 memblock_env
    - 给每个 agent sqr.main_phase 设置 default_sequence
  调用:
    uvm_config_db::get(this, "", "vif", vif)
    memblock_env::type_id::create("env", this)
    uvm_config_db::set(... default_sequence ...)

memblock_env::build_phase(phase)
  输入: uvm_phase
  输出/副作用:
    - 获取或创建 memblock_env_cfg
    - apply_user_cfg()
    - 创建 agent、monitor fifo、rm、scb
    - 把 agent cfg 写入 uvm_config_db
  调用:
    uvm_config_db#(memblock_env_cfg)::get()
    cfg.randomize()
    cfg.apply_user_cfg()
    <agent>::type_id::create()
    uvm_config_db#(<agent_cfg>)::set()

memblock_env::connect_phase(phase)
  输入: uvm_phase
  输出/副作用:
    - agent monitor analysis_port 连接到 RM FIFO
    - RM expected/actual 连接到 scb FIFO
  调用:
    mon_item_port.connect(...)
    rm.<agent>_mon_item_port.connect(...)
```

`tc_base` 默认挂上 dispatch sequence；`tc_dispatch_real_smoke` 不覆盖 LSQENQ/LINTSISSUE/LSQCOMMIT/REDIRECT/L2TLB 这几个 dispatch sequence，因此真实 flow 中这些 sequence 会和 `memblock_main_dispatch_auto_build_main_table_base_sequence` 并行工作。

### A.2 参数初始化函数

| 函数 | 输入 | 输出/副作用 | 功能 | 子函数 |
|---|---|---|---|---|
| `seq_csr_common::init()` | 无 | 加载 plus 参数，设置 `initialized=1` | sequence 使用参数前的统一入口；带 semaphore 和 initialized guard，避免并发重复解析 | `plus::reload_from_cmdline()`、`load_from_plus()`、`validate_and_clamp()` |
| `seq_csr_common::reload_from_plus()` | 无 | 强制重新加载 plus 参数并设 initialized | testcase build phase 用于应用 cfg/plus preset 后刷新参数快照 | `plus::reload_from_cmdline()`、`load_from_plus()`、`validate_and_clamp()` |
| `seq_csr_common::load_from_plus()` | 无 | 把 `plus::MEMBLOCK_*` 写入小写静态字段 | 参数落地，后续只通过 getter 读取 | `get_non_negative_int()` |
| `seq_csr_common::validate_and_clamp()` | 无 | fatal 非法值，clamp 超范围值，warning 可修复配置 | 防止随机约束权重全 0、pipe 数超过真实硬件数、timeout 为 0 等问题 | `fatal_if_zero()`、`fatal_if_all_zero2/3/5()`、`clamp_int()` |
| `seq_csr_common::check_initialized(caller)` | caller 字符串 | 未初始化则 fatal | 保护 transaction randomize 和 getter 调用时机 | 无 |
| `seq_csr_common` getter | 无 | 返回参数快照 | 按类别提供主表规模、pipe 数、权重、TLB、issue、LSQ、L2TLB、replay、redirect、timeout 参数 | `check_initialized()` |

重要参数类别：

| 类别 | 代表参数 | 在流程中的作用 |
|---|---|---|
| 主表生成 | `main_trans_num`、`use_manual_main_table`、`op_class_*_wt` | 决定生成多少 transaction、随机还是手动主表、load/store/AMO/prefetch 比例 |
| LSQ admission | `enq_per_cycle`、`enq_per_cycle_rand_en`、`real_lsq_enq_max`、`real_enq_width`、`lsqenq_*` | 控制每拍最多入队数、是否随机化入队数量、真实 8-wide slot 镜像、LSQ enqueue sequence 是否工作和 timeout |
| issue 调度 | `load_pip_num_limit`、`sta_pip_num_limit`、`std_pip_num_limit`、`send_pri_mode_en`、`global_send_pri_en_wt`、`send_pri_*` | 控制 LOAD/STA/STD 每拍发射上限、是否比较 priority、是否按权重启用全局优先级过滤 |
| 字段补充 | `mdp_load_wait_wt`、`mdp_storeset_hit_wt`、`load_wait_strict_wt`、`pc_base`、`pdest_*` | 控制发射前补充的 MDP、pc、pdest、rfWen/fpWen 等字段 |
| TLB/L2TLB | `tlb_pte_*_wt`、`paddr_base`、`paddr_range`、`l2tlb_*` | 控制 PTE bit 权重、物理地址范围、L2TLB responder 查表和延迟策略 |
| replay/redirect | `redirect_seq_en`、`redirect_*_timeout`、`replay_wait_ptw_en`、`replay_wait_ptw_timeout` | 控制 redirect 是否真实驱动、等待 PTW 后 replay 的策略 |
| commit/flushSb | `lsqcommit_*`、`flushsb_seq_en`、`flushsb_request_cycle`、`flushsb_timeout` | 控制 pendingPtr 驱动、flushSb 发起和等待 sbIsEmpty 的超时 |

## 3. 阶段 B：主表生成与主状态初始化

### B.1 主表生成调用链

```text
memblock_main_dispatch_auto_build_main_table_base_sequence::body()
  -> build_main_table()
       -> if get_use_manual_main_table()
            import_manual_main_table()
          else
            build_random_main_table(get_main_trans_num())
  -> service_real_dispatch_flow()
```

随机主表：

```text
build_random_main_table(main_trans_num_i)
  输入: 主表行数
  输出/副作用:
    - data.reset_all_tables(main_trans_num_i)
    - 连续分配 uid 0..N-1
    - ROB key 从可配置起始 value 开始按 ROB 环形规则递增，初始 flag 固定为 0
    - 写 main_table_by_uid[uid]
    - 初始化 status_by_uid[uid]
    - main_table_ready = 1
  调用:
    data.reset_all_tables()
    data.alloc_uid()
    main_control_transaction::type_id::create()
    randomize_main_transaction()
    prune_recent_uid_q()
    apply_addr_reuse_window()
    data.set_main_transaction()
    push_recent_uid()
    rob_order_util::rob_advance()
    init_status_for_main_table()
    data.check_main_table_complete()
```

手动主表：

```text
import_manual_main_table()
  输入: manual_main_table_by_rob[int unsigned]
  输出/副作用:
    - 按 rob_key 排序导入主表
    - 每条 transaction 分配 uid
    - validate 后写 main_table_by_uid
  调用:
    data.reset_all_tables(manual_num)
    data.alloc_uid()
    tr.post_manual_config()
    validate_main_table_entry()
    data.set_main_transaction()
    init_status_for_main_table()
    data.check_main_table_complete()
```

### B.2 主表生成相关函数

| 函数 | 输入 | 输出/副作用 | 功能 | 子函数 |
|---|---|---|---|---|
| `memblock_main_dispatch_auto_build_main_table_base_sequence::body()` | 无 | 构建主表、服务全流程、最终一致性检查 | real DUT dispatch smoke 的主控入口；它不直接驱动各 DUT 接口，而是启动主表后用 service loop 消费 monitor raw event、推进状态、检查完成 | `build_main_table()`、`service_real_dispatch_flow()`、`data.end_test_check()` |
| `service_real_dispatch_flow()` | 无 | 每个 service clock 下降沿服务 monitor/handler/route | 主控软件服务循环；获取 lintsissue agent interface 作为 service clock，每个下降沿先处理 DUT 输出，再把 ready uid 重新 route 到 issue queue，直到全部 terminal_done | `ensure_service_vif()`、`service_monitor_once()`、`route_all_issue_queues()`、`all_transactions_terminal_done()` |
| `service_monitor_once()` | 无 | raw monitor queue 被消费，状态表推进 | 一拍 TB 软件服务周期；统一 drain CSR runtime、sfence/hfence、writeback、ctrl、redirect/replay | `tick_dispatch_service_cycle()`、`collect_runtime_context_events()`、`collect_writeback_events()`、`collect_exception_and_redirect_events()`、`exception_redirect_replay_task()` |
| `all_transactions_terminal_done()` | 无 | bit | 完成判据；要求 issue queue/active map/redirect/flushSb/PTW wait 都空，所有 uid 必须 terminal_done，fault/exception 可为 success=0 的非成功终态 | `data.get_status()`、`data.has_pending_redirect_drive()` |
| `report_unfinished_status()` | 无 | 打印未完成 uid 状态和关键 HDL 信号 | timeout debug 入口；把状态表字段、主表字段和部分 DUT 内部信号打印出来，定位卡在 admission/issue/TLB/writeback/commit/deq 哪一步 | `report_main_transaction()`、`report_hdl_bit()`、`report_hdl_value()` |
| `report_main_transaction(uid)` | uid | 打印该 uid 主表字段 | timeout 时补充静态激励信息，如 `op_class/fuType/fuOpType/src/imm/rob/lq/sq/send_pri` | `data.main_table_by_uid[]` |
| `report_hdl_bit(path)` / `report_hdl_value(path)` | HDL 路径 | 打印 bit 或 value；不可读则 info | timeout 时用 UVM HDL read 直接观察 DUT 关键内部信号 | `uvm_hdl_read()` |
| `memblock_dispatch_base_sequence::pre_body()` | 无 | 初始化 `data/lsq_ctrl/tlb_builder/issue_sched/field_assigner/writeback_handler/exception_handler/monitor_commit_handler/monitor_adapter` | 所有 helper 句柄准备入口 | `seq_csr_common::init()`、`common_data_transaction::get()`、`lsq_ctrl_model::get()`、各 helper `type_id::create()`、`monitor_commit_handler.bind_lsq_ctrl()`、`monitor_adapter.bind_commit_handler()` |
| `build_main_table()` | 无 | 主表 ready | 按参数选择随机生成或手动导入 | `get_use_manual_main_table()`、`import_manual_main_table()`、`build_random_main_table()` |
| `build_random_main_table(main_trans_num_i)` | 行数 | 主表、status 表、ROB 顺序 | 随机生成所有 transaction | `reset_all_tables()`、`alloc_uid()`、`randomize_main_transaction()`、`prune_recent_uid_q()`、`apply_addr_reuse_window()`、`set_main_transaction()`、`push_recent_uid()`、`rob_advance()`、`init_status_for_main_table()`、`check_main_table_complete()` |
| `import_manual_main_table()` | `manual_main_table_by_rob` | 主表、status 表 | 吃手动配置关联数组，不随机主表 | `reset_all_tables()`、`alloc_uid()`、`post_manual_config()`、`validate_main_table_entry()`、`set_main_transaction()` |
| `randomize_main_transaction(tr, uid, rob_key)` | transaction、uid、ROB key | 填 `tr` 所有主表字段 | 先 randomize，再用模板修正为合法 load/store/AMO/prefetch | `tr.randomize()`、`select_op_class_by_weight()`、`apply_minimal_op_template()`、`apply_legal_addr_template()`、`randomize_send_pri_value()`、`randomize_delay_value()`、`tr.update_vaddr()`、`validate_main_table_entry()` |
| `select_op_class_by_weight()` | 无 | `memblock_op_class_e` | 按 plus 权重选择 INT load、FP load、store、prefetch、AMO | `rand_weighted5()`、`get_op_class_*_wt()` |
| `apply_minimal_op_template(tr)` | 主表 transaction | 修正 `fuType/fuOpType/lsq_flow/numLsElem` | 根据 `op_class` 建立最小合法模板 | `random_load_fuoptype()`、`random_store_fuoptype()`、`random_prefetch_fuoptype()`、`random_amo_fuoptype()` |
| `apply_legal_addr_template(tr)` | 主表 transaction | 修正 `src_0/imm/vaddr` | 在 `paddr_base/range` 内按 64B slot 选择地址，避免无效地址 | `get_paddr_base()`、`get_paddr_range()`、`tr.update_vaddr()` |
| `choose_rob_start_key()` | 无 | uid0 ROB key | 选择 ROB 起始 value，初始 flag 固定为 0 | `get_rob_start_*()`、`rand_weighted3()` |
| `choose_addr_ref_window()` | 无 | uid 距离窗口 | 选择 recent queue 保留窗口 | `get_addr_ref_window_*()`、`rand_weighted3()` |
| `apply_addr_reuse_window()` | 当前 transaction、uid、recent load/store queue | 可能修正类型和 `src_0/imm/vaddr` | 主表生成期按 recent-window 提高地址相关概率 | `select_addr_reuse_kind()`、`random_pick_recent_uid()`、`set_transaction_ls_kind()`、`fixup_after_addr_reuse()` |
| `prune_recent_uid_q()` | recent queue、当前 uid、窗口 | 删除过期候选 uid | 确保候选仍在 uid 距离窗口内 | queue `pop_front()` |
| `push_recent_uid()` | 最终 transaction、uid | 当前 uid 入 load/store recent queue | 让后续 transaction 能复用当前地址 | `is_load_main_tr()`、`is_store_main_tr()` |
| `init_status_for_main_table()` | 无 | `status_by_uid[]` 初始化 | 每个 uid 建 status 并 snapshot 主表 ROB/LQ/SQ key | `data.init_status_for_uid()` |
| `validate_main_table_entry(tr, caller)` | transaction、调用者描述 | 非法则 fatal | 检查 vaddr、send_pri、ROB 范围、vector LS 禁用、op template 合法性 | `tr.validate_main_transaction()`、`derive_op_behavior()`、`is_*_fuoptype()` |

### B.3 主表 transaction 函数

| 函数 | 输入 | 输出/副作用 | 功能 | 子函数 |
|---|---|---|---|---|
| `main_control_transaction::pre_randomize()` | 无 | 未 init 则 fatal | 保证随机约束使用 plus 前参数已加载 | `seq_csr_common::check_initialized()` |
| `post_randomize()` | 无 | 更新 `vaddr`，非法则 fatal | randomize 后合法性修正 | `update_vaddr()`、`validate_main_transaction()` |
| `post_manual_config(recompute_vaddr)` | 是否重算 vaddr | 手动主表合法化 | 手动主表导入后修正/校验 | `update_vaddr()`、`validate_main_transaction()` |
| `update_vaddr()` | 无 | `vaddr = src_0 + sign_extend_imm12(imm)` | 保持地址派生字段一致 | `sign_extend_imm12()` |
| `validate_main_transaction()` | 无 | bit | 检查 vaddr 和 send_pri 范围 | 无 |
| `get_rob_key()` | 无 | `{flag,value}` | 从主表导出 ROB 环形 key | 无 |
| `get_lq_key()` | 无 | `{flag,value}` | 从主表导出 LQ 环形 key | 无 |
| `get_sq_key()` | 无 | `{flag,value}` | 从主表导出 SQ 环形 key | 无 |
| `sign_extend_imm12(imm_i)` | 64b imm | 64b sign-extended imm12 | 按 12-bit 立即数规则扩展 | 无 |

## 4. 阶段 C：LSQ Admission / 入队

LSQ admission 的职责是把主表里的 uid 送到 DUT 的 `enqLsq` 接口，同时让软件 LSQ 模型和 DUT 返回的 LQ/SQ index 对齐。此阶段完成后，uid 才变成 active，并进入 TLB/issue 后续流程。

### C.1 LSQ 入队主调用链

```text
memblock_lsqenq_dispatch_base_sequence::body()
  -> seq_csr_common::init()
  -> configure_from_plus()
  -> if !enable: super.body(); return
  -> ensure_helpers()
  -> wait_for_main_table()
  -> drive_lsqenq_loop()

drive_lsqenq_loop()
  forever loop, cycle_idx++:
    send_lsqenq_cycle(cycle_idx, has_progress)
    idle_count control; only idle_stop exits

send_lsqenq_cycle(cycle_idx, has_progress)
  -> admit_non_lsq_if_ready(has_progress)
       if behavior.need_alloc == 2'b00:
         lsq_ctrl.commit_non_lsq_admission()
         complete_admission()
         data.mark_uid_enqueued()/公共 max_enqueued_uid 前进
         return
  -> collect_lsq_candidates(uids, trs, behaviors, lq_keys, sq_keys)
  -> create lsqenq xaction
  -> clear_lsqenq_xaction(tr)
  -> set wait_can_accept / ready_timeout / flush_epoch
  -> foreach candidate: assign_lsqenq_slot()
       -> set_need_alloc()
       -> set_req_fields()
  -> start_item(tr); finish_item(tr)
       driver wait_lsq_can_accept():
         while not canAccept:
           if flush epoch changed: abort
           drive req
         sample_lsqenq_resp()
  -> confirm_lsq_candidates()
       -> get_resp_keys()
       -> lsq_ctrl.commit_allocate_with_resp()
       -> complete_admission()
```

### C.2 `memblock_lsqenq_dispatch_base_sequence` 函数表

| 函数 | 输入 | 输出/副作用 | 功能 | 子函数 |
|---|---|---|---|---|
| `body()` | 无 | 启动或退回默认 sequence | LSQ enqueue sequence 主入口 | `seq_csr_common::init()`、`configure_from_plus()`、`ensure_helpers()`、`wait_for_main_table()`、`drive_lsqenq_loop()` |
| `configure_from_plus()` | 无 | 设置 `enable/idle_stop/ready_timeout/start_timeout` | 从参数快照读取 LSQENQ 控制；无专用 max_cycles | `get_lsqenq_*()` |
| `ensure_helpers()` | 无 | 获取 `data/lsq_ctrl/tlb_builder/issue_sched/monitor_adapter` | 准备入队后需要的公共 helper | `common_data_transaction::get()`、`lsq_ctrl_model::get()`、`type_id::create()` |
| `wait_for_main_table()` | 无 | 等 `data.main_table_ready` | 防止 agent sequence 早于主表生成 | `#1` loop |
| `drive_lsqenq_loop()` | 无 | 多拍发送 LSQ admission | 每拍调用 `send_lsqenq_cycle`，idle 到阈值退出 | `send_lsqenq_cycle()` |
| `send_lsqenq_cycle(cycle_idx, has_progress)` | cycle 编号 | xaction 发给 driver；成功后状态推进 | 一拍 admission 的完整逻辑 | `admit_non_lsq_if_ready()`、`collect_lsq_candidates()`、`clear_lsqenq_xaction()`、`assign_lsqenq_slot()`、`start_item()`、`finish_item()`、`confirm_lsq_candidates()` |
| `admission_blocked_by_flush()` | 无 | bit | redirect/flush 期间禁止 admission | `data.issue_blocked_by_global_flush()` |
| `next_uid_needs_lsq_admission(uid, main_tr, behavior)` | output uid/tr/behavior | 找到下一个未 enq uid | 从 `data.get_next_new_admit_uid()` 取得公共 admission 起点，跳过已 enq，遇到 active/pending/flushed 停止 | `admission_blocked_by_flush()`、`data.get_status()`、`data.get_main_transaction()`、`lsq_ctrl_model::derive_op_behavior()` |
| `collect_lsq_candidates(uids, trs, behaviors, lq_keys, sq_keys)` | output 队列 | 本拍候选 uid 和预览 LQ/SQ key | 从 `data.get_next_new_admit_uid()` 起按连续 uid 收集；受 `get_enq_per_cycle()` 固定/随机上限、LQ/SQ free count 和 flush 状态约束 | `next_uid_needs_lsq_admission()`、`advance_lq_key()`、`advance_sq_key()` |
| `clear_lsqenq_xaction(tr)` | xaction | 清空 `needAlloc/req/canAccept` | 防止上拍字段残留 | `set_need_alloc()`、`set_req_fields()` |
| `assign_lsqenq_slot(tr, slot, uid, main_tr, behavior, lq_key, sq_key)` | xaction、slot、候选信息 | 填 slot 的 needAlloc 和 req payload | 把主表 ROB/fuType、预览 LQ/SQ 写入对应 enq slot | `set_need_alloc()`、`set_req_fields()` |
| `set_need_alloc(tr, slot, need_alloc)` | slot、need_alloc | 写 `io_ooo_to_mem_enqLsq_needAlloc_N` | 指示本 slot 是否需要 LQ/SQ 分配 | 无 |
| `set_req_fields(tr, slot, valid, fuType, uopIdx, rob_key, lq_key, sq_key, numLsElem)` | slot 和 payload | 写 `enqLsq_req_N` 字段 | 驱动 DUT admission 请求字段 | 无 |
| `get_resp_keys(tr, slot, lq_key, sq_key)` | xaction、slot | 输出 DUT 返回 LQ/SQ key | 从 driver 采回的 resp 字段取实际 index | 无 |
| `confirm_lsq_candidates(tr, uids, trs, behaviors, has_progress)` | xaction 和候选列表 | 确认分配，更新软件状态 | flush 中断则不确认本批分配；redirect/flush 处理函数会回退公共 admission 边界。未中断时检查 DUT resp 并 commit 软件 LSQ | `get_resp_keys()`、`lsq_ctrl.commit_allocate_with_resp()`、`complete_admission()` |
| `complete_admission(uid)` | uid | TLB 表 ready，issue queue 路由 | 入队成功后的后处理 | `drain_csr_runtime_events()`、`tlb_builder.build_tlb_for_uid()`、`issue_sched.route_uid()` |
| `admit_non_lsq_if_ready(has_progress)` | output progress | 非 LSQ 分配类操作直接 active | 目前 AMO 简化为 `need_alloc=0`，但仍会入 issue route | `next_uid_needs_lsq_admission()`、`commit_non_lsq_admission()`、`complete_admission()` |

### C.3 `lsq_ctrl_model` 函数表

| 函数 | 输入 | 输出/副作用 | 功能 | 子函数 |
|---|---|---|---|---|
| `get()` | 无 | 单例句柄 | 获取软件 LSQ 模型 | `new()` |
| `reset()` | 无 | LQ/SQ enq/deq ptr 清 0，free count 复位 | 测试开始或主表 reset 时恢复模型 | 无 |
| `derive_op_behavior(tr)` | 主表 transaction | `memblock_op_behavior_t` | 从 `fuType/fuOpType` 推导是否用 LQ/SQ、路由到哪些 issue target、是否 load/store/prefetch/CBO/atomic | `is_vector_ls_futype()`、`is_load_fuoptype()`、`is_prefetch_fuoptype()`、`is_store_fuoptype()`、`is_cbo_fuoptype()`、`is_amo_fuoptype()`、`is_amocas_*()`、`make_default_behavior()` |
| `make_default_behavior()` | 无 | 默认 behavior | 初始化所有 route/alloc/commit 标志 | 无 |
| `is_*_fuoptype()` | fuOpType | bit | 判断 fuOpType 分类 | 无 |
| `advance_lq_key(base, step)` | LQ key、步数 | 新 LQ key | 按 LQ size 回绕并翻转 flag | 无 |
| `advance_sq_key(base, step)` | SQ key、步数 | 新 SQ key | 按 SQ size 回绕并翻转 flag | 无 |
| `rewind_lq_key(base, step)` | LQ key、步数 | 回退后的 LQ key | 用于 DUT deq ptr 表示 next ptr 时反推出队起点 | 无 |
| `rewind_sq_key(base, step)` | SQ key、步数 | 回退后的 SQ key | 同上，用于 SQ | 无 |
| `can_allocate(behavior)` | behavior | bit | 检查 LQ/SQ free count 是否足够 | 无 |
| `preview_allocate(behavior, lq_key, sq_key)` | behavior | 输出当前 enq ptr | 不改变状态，只预览本次分配起点 | `can_allocate()` |
| `commit_allocate(uid, behavior, tr)` | uid、behavior、主表 | 写主表 LQ/SQ，激活 uid，推进 enq ptr/free count | 软件自洽分配路径 | `preview_allocate()`、`data.set_main_transaction()`、`data.activate_uid()`、`data.set_status_field()`、`advance_lq_key()`、`advance_sq_key()` |
| `commit_allocate_with_resp(uid, behavior, tr, dut_lq_key, dut_sq_key)` | uid、behavior、主表、DUT resp key | 检查 DUT resp 与软件预览一致，然后 commit | 真实 LSQ admission 确认路径 | `preview_allocate()`、`data.set_main_transaction()`、`data.activate_uid()`、`data.set_status_field()`、`advance_lq_key()`、`advance_sq_key()` |
| `commit_non_lsq_admission(uid, behavior, tr)` | uid、behavior、主表 | 非 LQ/SQ 分配类操作 active/enq | 复用 `commit_allocate`，要求 `need_alloc=0` | `commit_allocate()` |
| `release_lq(count)` | 出队数量 | 推进 LQ deq ptr，增加 free count | DUT LQ deq monitor 确认后释放软件资源 | `advance_lq_key()` |
| `release_sq(count)` | 出队数量 | 推进 SQ deq ptr，增加 free count | DUT SQ deq monitor 确认后释放软件资源 | `advance_sq_key()` |
| `cancel_lq(count)` / `cancel_sq(count)` | cancel 数量 | 回退 enq ptr，增加 free count | 为 redirect/cancel 类扩展保留的资源回退 helper | `rewind_lq_key()`、`rewind_sq_key()` |

## 5. 阶段 D：TLB 表生成与 L2TLB responder

### D.1 TLB 生成调用链

```text
complete_admission(uid)
  -> drain_csr_runtime_events()
       -> monitor_adapter.drain_csr_events()
          -> memblock_sync_pkg::get_latest_raw_csr()
          -> data.apply_raw_csr_runtime(raw_csr, raw_csr_seq)
  -> data.set_status_field(uid, TLB_MAPPED, 1)
  -> route_issue_queue_for_uid(uid)

send_l2tlb_cycle(...)
  -> data.get_or_create_tlb_entry_by_req(vpn, s2xlate, key, entry, created)
       -> data.make_tlb_key_by_req(vpn, s2xlate)
       -> data.build_tlb_entry_for_key(key)  // miss only
       -> tlb_map_builder.build_tlb_entry_for_req(key.vpn, key.s2xlate, runtime_csr)
```

### D.2 L2TLB responder 调用链

```text
memblock_l2tlb_base_sequence::body()
  -> seq_csr_common::init()
  -> configure_from_plus()
  -> if !enable return
  -> ensure_context()
  -> if !memblock_sync_pkg::l2tlb_responder_active fatal
  -> drive_l2tlb_loop()

drive_l2tlb_loop()
  loop:
    @(l2tlb_vif.drv_cb)
    send_l2tlb_cycle(idle_count, has_progress)
    if has_progress: idle_count = 0
    else: idle_count++ until idle_count > idle_stop_cycle

send_l2tlb_cycle(idle_count, has_progress)
  -> if request_valid()
       sample_request_fields(vpn, s2xlate)
       send ready_tr
       resp_tr = create_l2tlb_xaction()
       resp_tr.pre_pkt_gap = choose_latency()
       resp_tr req echo fields
       data.get_or_create_tlb_entry_by_req(vpn, s2xlate, key, entry, created)
       fill_dtlb_resp_from_entry(entry, resp_tr)
       data.update_uid_tlb_records_by_entry(key, entry)
       send_l2tlb_item(resp_tr)
```

L2TLB agent 代替的是 L2TLB 对上游 DTLB 的 responder 功能，连接点是 DTLB 与 L2TLB 之间的 request/response，不是 L2TLB 到 L2Cache/PTW 下游模型。查表 key 使用 DTLB request 中的 `vpn/s2xlate`，ASID/VMID 来自 runtime CSR 镜像。

### D.3 TLB/L2TLB 函数表

| 函数 | 输入 | 输出/副作用 | 功能 | 子函数 |
|---|---|---|---|---|
| `tlb_map_builder::build_tlb_entry_for_req(vpn, s2xlate, csr_state)` | request vpn/s2xlate、runtime CSR | `memblock_tlb_entry` | miss 时生成 by-key TLB/PTE 表项 | `choose_paddr()`、`randomize_pte_bits()`、`fixup_pte_legal()`、`apply_csr_state()` |
| `choose_paddr(vaddr)` | vaddr | paddr | 按 plus 物理地址范围把 vaddr 映射到 paddr | `get_paddr_base()`、`get_paddr_range()` |
| `randomize_pte_bits(entry)` | TLB entry | 修改 PTE bit | 根据 plus 权重随机 PTE 权限/属性 | `choose_weighted_bit()`、`get_tlb_pte_*_wt()` |
| `choose_weighted_bit(one_wt, zero_wt)` | 权重 | bit | 二值权重选择 | 无 |
| `memblock_tlb_entry::update_addr_fields(vaddr_i, paddr_i)` | vaddr/paddr | 更新 vpn/ppn/addr_low/index | 派生 L2TLB response 所需字段 | `derive_index_fields()` |
| `memblock_tlb_entry::apply_csr_state(csr_state, s2xlate_i)` | runtime CSR、s2xlate | 写 asid/vmid/priv/lookup_key | 将当前 MMU CSR 上下文绑定到 TLB 表项 | `make_lookup_key()` |
| `memblock_tlb_entry::fixup_pte_legal()` | 无 | 修正 PTE bit | 保证 V/R/W/X/A/D 基本合法组合 | 无 |
| `mmu_csr_runtime_state::update_from_raw_csr(raw)` | raw CSR | 更新 CSR 镜像，变化则 `update_seq++` | 运行时获取真实 DUT CSR，而不是初始静态 CSR | 无 |
| `mmu_csr_runtime_state::make_lookup_key(vpn, s2xlate)` | vpn、s2xlate | `{vpn,asid,vmid,s2xlate}` | 生成 TLB 关联数组 key | `current_asid()`、`current_vmid()` |
| `memblock_l2tlb_base_sequence::body()` | 无 | 启动或关闭 L2TLB responder | L2TLB sequence 主入口；确认 plus enable 和 connect takeover 后直接进入 request responder loop | `seq_csr_common::init()`、`configure_from_plus()`、`ensure_context()`、`drive_l2tlb_loop()` |
| `drive_l2tlb_loop()` | 无 | 多拍采样 request 并发 response | L2TLB responder 主循环；每拍在 `drv_cb` 上调用一次 `send_l2tlb_cycle()`，有 progress 则清 `idle_count`，连续 idle 超过 `idle_stop_cycle` 后退出 | `send_l2tlb_cycle()` |
| `send_l2tlb_cycle(idle_count, has_progress)` | idle 计数 | 可能发 ready/response xaction | 单次 request-response 闭环；采到 request 后先发 ready，再按 latency 设置 response gap，查/建 TLB 表并发 response | `request_valid()`、`sample_request_fields()`、`create_l2tlb_xaction()`、`send_l2tlb_item()`、`choose_latency()`、`get_or_create_tlb_entry_by_req()`、`fill_dtlb_resp_from_entry()`、`update_uid_tlb_records_by_entry()` |
| `send_l2tlb_item(tr)` | L2TLB xaction | 发给 driver | L2TLB sequence 到 agent driver 的统一发送入口 | `start_item()`、`finish_item()` |
| `configure_from_plus()` | 无 | 设置 enable、latency、idle stop cycle | 从 `seq_csr_common` 读取 L2TLB responder 参数 | `get_l2tlb_*()` |
| `ensure_context()` | 无 | 获取 `data/monitor_adapter/l2tlb_vif` | 响应 request 前准备公共数据和 interface；vif 不存在则 fatal | `common_data_transaction::get()`、`type_id::create()`、`uvm_config_db::get()` |
| `request_fire()` | 无 | bit | 判断 request valid 和 ready 同时为 1；当前 responder 主要用 `request_valid()` 接收 request，`request_fire()` 是握手辅助判断 | 读 `l2tlb_vif`、`reset_backend_done` |
| `memblock_l2tlb_base_sequence::request_valid()` | 无 | bit | 判断 DTLB request valid 且 reset/backend ready | 读 `l2tlb_vif`、`memblock_sync_pkg::reset_backend_done` |
| `create_l2tlb_xaction(name)` | xaction 名称 | 已清空的 xaction | 创建 L2TLB driver transaction，创建失败 fatal | `type_id::create()`、`clear_l2tlb_xaction()` |
| `clear_l2tlb_xaction(tr)` | xaction | 清 request ready/response valid/response payload | 防止 ready/wait/response transaction 字段残留 | 无 |
| `sample_request_fields(vpn, s2xlate)` | output | DTLB request 字段 | 从接口采样 lookup 输入 | 读 `l2tlb_vif` |
| `get_or_create_tlb_entry_by_req(vpn, s2xlate, key, entry, created)` | request vpn/s2xlate | key、entry、created | 用 runtime CSR 生成 by-key TLB key；命中返回已有 entry，miss 自动创建 entry | `make_tlb_key_by_req()`、`build_tlb_entry_for_key()`、`insert_tlb_entry()` |
| `fill_dtlb_resp_from_entry(entry, resp)` | response xaction、TLB entry | 写 L2TLB response 字段 | 把 PTE/PPN/权限/fault 字段回填给 DTLB | 无 |
| `update_uid_tlb_records_by_entry(key, entry)` | key、entry | 回填匹配 uid record | 将 response 的 PTE 信息同步到所有匹配且尚未完成的 uid 追踪记录 | `memblock_uid_tlb_record::copy_entry_fields()` |
| `choose_latency()` | 无 | 延迟拍数 | L2TLB responder 随机响应延迟 | `$urandom_range()` |

## 6. 阶段 E：issue queue 路由与调度

### E.1 路由调用链

```text
complete_admission(uid)
  -> issue_sched.route_uid(uid)

memblock_main_dispatch_auto_build_main_table_base_sequence::service_real_dispatch_flow()
  每个 service clock 下降沿:
    service_monitor_once()
    route_all_issue_queues()
       -> issue_sched.route_all_ready_uids()

memblock_issue_dispatch_base_sequence::drive_dispatch_issue_loop()
  loop:
    issue_sched.route_all_ready_uids()
    send_issue_cycle()
    issue_sched.advance_issue_queue_delays()
```

### E.2 `issue_queue_scheduler` 路由函数表

| 函数 | 输入 | 输出/副作用 | 功能 | 子函数 |
|---|---|---|---|---|
| `make_empty_item()` | 无 | 空 `memblock_issue_q_item_t` | 初始化 issue queue item | 无 |
| `make_issue_item(uid, target, behavior)` | uid、target、behavior | item | 从主表/status 生成轻量队列项，包含 `uid/rob_key/target/send_pri/ready_cycle/replay_seq/lq/sq/uop_count` | `data.get_main_transaction()`、`data.get_status()` |
| `is_uid_route_ready(uid)` | uid | bit | 判断 uid 是否可路由：active、enq、tlb_mapped、未 flushed/redirect/exception，replay 时仅允许 replay target | `data.issue_blocked_by_global_flush()`、`data.get_status()` |
| `target_already_queued_or_done(status, target)` | status、target | bit | 防重复入队；已 queued/dispatched/pass 的 target 不再入队 | 无 |
| `set_target_queued(uid, target, value)` | uid、target、bit | 修改 status queued bit | 统一更新 queued_load/sta/std | `data.set_status_field()` |
| `set_target_dispatched(uid, target, value)` | uid、target、bit | 修改 status dispatched bit | 统一更新 dispatched bit | `data.set_status_field()` |
| `route_target(uid, target, behavior)` | uid、target、behavior | push issue queue，置 queued | 单 target 入队；先删除旧项防重复 | `target_already_queued_or_done()`、`data.replay_target_requested()`、`data.delete_issue_queue_entry()`、`make_issue_item()`、`data.push_issue_queue_item()`、`set_target_queued()` |
| `route_uid(uid)` | uid | LOAD/STA/STD 队列更新 | 按 behavior 把 uid 路由到对应 target | `is_uid_route_ready()`、`lsq_ctrl_model::derive_op_behavior()`、`route_target()` |
| `route_all_ready_uids()` | 无 | 扫描所有 uid 并尝试路由 | monitor/replay 后兜底把 ready uid 送回 issue queue | `data.issue_blocked_by_global_flush()`、`route_uid()` |
| `advance_issue_queue_delays()` | 无 | 每个队列 item 的 `ready_cycle--` | 实现发射延迟 | 无 |

### E.3 调度选择调用链

```text
memblock_issue_dispatch_base_sequence::send_issue_cycle()
  -> issue_sched.select_issue_candidates(load_items, sta_items, std_items)
       -> if issue_blocked_by_global_flush: issue_freeze_ack=1; return
       -> compare_pri = get_send_pri_mode_en()
       -> use_global_pri = compare_pri && sample_global_send_pri_en()
       -> if use_global_pri && !find_global_max_send_pri(global_pri): use_global_pri = 0
       -> select_target_candidates(LOAD, sample_load_pip_num(), compare_pri, use_global_pri, global_pri, load_items)
       -> select_target_candidates(STA,  sample_sta_pip_num(),  compare_pri, use_global_pri, global_pri, sta_items)
       -> select_target_candidates(STD,  sample_std_pip_num(),  compare_pri, use_global_pri, global_pri, std_items)
```

| 函数 | 输入 | 输出/副作用 | 功能 | 子函数 |
|---|---|---|---|---|
| `is_issue_item_eligible(item)` | item | bit | 判断 item 当前是否能发射，受全局 flush 阻塞 | `data.issue_blocked_by_global_flush()`、`is_issue_item_state_eligible()` |
| `is_issue_item_state_eligible(item)` | item | bit | 检查 active/enq/tlb、未 killed/flushed/pending、`replay_seq` 匹配、delay 到 0、target 未 dispatched/done | `data.get_status()`、`data.replay_target_requested()` |
| `item_is_older(left, right)` | 两个 item | bit | 比较 ROB 年龄；同 ROB 时 uid 小优先 | `rob_order_util::rob_is_after()` |
| `item_is_better(candidate, best, compare_pri)` | 两个 item、是否比较优先级 | bit | send_pri 开启时先比优先级，再比年龄；不开时只比年龄 | `item_is_older()` |
| `index_already_selected(idx, selected_indices)` | index、已选列表 | bit | 防止同一 target 队列重复选同一项 | 无 |
| `get_target_queue_size(target)` | target | 队列长度 | 统一读取 load/sta/std 队列长度 | 无 |
| `get_target_queue_item(target, idx)` | target、index | item | 统一读取某 target 队列 item | 无 |
| `find_global_max_send_pri(max_pri)` | output | 是否找到最大优先级 | 在三个队列所有 eligible item 中找最大 `send_pri` | `is_issue_item_eligible()` |
| `select_target_candidates(target, max_count, use_global_pri, global_pri, selected)` | target、最多数量、优先级模式 | selected 队列 | 在一个 target 队列内选最多 `max_count` 个 item | `get_target_queue_size()`、`get_target_queue_item()`、`is_issue_item_eligible()`、`item_is_better()` |
| `select_issue_candidates(load_items, sta_items, std_items)` | output 三个候选队列 | 每拍发射候选 | `send_pri_mode_en=1` 时队列内比较 priority；`sample_global_send_pri_en()` 命中时三个 target 只选全局最大优先级；否则各 target 并行按 pipe 数选择 | `find_global_max_send_pri()`、`select_target_candidates()` |
| `mark_issue_fire(item)` | item | 删除队列项，置 dispatched，分配 issue_epoch | 正常发射成功后的状态更新 | `is_issue_item_eligible()`、`data.alloc_issue_epoch()`、`data.mark_issue_snapshot()`、`data.delete_issue_queue_entry()`、`set_target_queued()`、`set_target_dispatched()`、`data.clear_replay_target_after_fire()` |
| `mark_issue_fire_already_accepted(item)` | item | 同上，但不因全局 flush 阻塞 | driver 已经部分 fire 后遇到 redirect abort 时使用，避免丢失已被 DUT 接收的 item | `is_issue_item_state_eligible()`、`data.alloc_issue_epoch()`、`data.mark_issue_snapshot()`、`data.delete_issue_queue_entry()`、`set_target_queued()`、`set_target_dispatched()` |

## 7. 阶段 F：issue 发射与字段填充

### F.1 发射调用链

```text
memblock_issue_dispatch_base_sequence::body()
  -> seq_csr_common::init()
  -> configure_from_plus()
  -> if !enable: super.body(); return
  -> ensure_helpers()
  -> wait_for_main_table()
  -> drive_dispatch_issue_loop()

drive_dispatch_issue_loop()
  loop:
    issue_sched.route_all_ready_uids()
    send_issue_cycle(cycle_idx, has_fire)
    issue_sched.advance_issue_queue_delays()

send_issue_cycle(cycle_idx, has_fire)
  -> create lintsissue xaction
  -> field_assigner.clear_lintsissue_xaction(tr)
  -> 设置 wait_ready / nonblocking / timeout / flush_epoch / fired_mask
  -> if !issue_blocked_by_global_flush():
       issue_sched.select_issue_candidates()
       assign_issue_items(load_items)
       assign_issue_items(sta_items)
       assign_issue_items(std_items)
  -> start_item(tr); finish_item(tr)
       driver:
         if nonblocking:
           drive_dispatch_issue_one_cycle()
           sample ready once, set fired_mask only for valid&&ready
           clear remaining valid and return
         else:
           wait_dispatch_issue_ready()
           while pending valid:
             clear ready ports that fired
             if dispatch_flush_epoch changed:
               clear valid, mark aborted
               return
             send_pkt
  -> if aborted_by_redirect:
       mark_fired_items(fired_items, fired_mask)
  -> else if no flush epoch change:
       mark_fired_items(fired_items, nonblocking ? fired_mask : 7'h7f)
```

### F.2 发射 sequence 函数表

| 函数 | 输入 | 输出/副作用 | 功能 | 子函数 |
|---|---|---|---|---|
| `memblock_issue_dispatch_base_sequence::body()` | 无 | 启动 issue sequence | 根据 plus enable 决定是否驱动真实 issue 口 | `seq_csr_common::init()`、`configure_from_plus()`、`ensure_helpers()`、`wait_for_main_table()`、`drive_dispatch_issue_loop()` |
| `configure_from_plus()` | 无 | 设置 enable/max/idle/start timeout | 读取 issue sequence 参数 | `get_dispatch_issue_*()` |
| `ensure_helpers()` | 无 | 准备 `data/issue_sched/field_assigner/issue_accept_wb_handler` | issue 发射所需 helper | `common_data_transaction::get()`、`type_id::create()` |
| `wait_for_main_table()` | 无 | 等主表 ready | 与主表生成同步 | 无 |
| `drive_dispatch_issue_loop()` | 无 | 多拍发射 | 每拍 route、send、advance delay | `route_all_ready_uids()`、`send_issue_cycle()`、`advance_issue_queue_delays()` |
| `send_issue_cycle(cycle_idx, has_fire)` | cycle | 驱动 issue xaction，更新发射状态 | 一拍 LOAD/STA/STD 发射 | `clear_lintsissue_xaction()`、`select_issue_candidates()`、`assign_issue_items()`、`start_item()`、`finish_item()`、`mark_fired_items()` |
| `assign_issue_items(tr, items, fired_items)` | xaction、候选 item 队列 | 填 xaction，并记录将要 fire 的 item | 将 scheduler 选出的 item 按队列内 idx 映射到对应 pipe | `field_assigner.assign_issue_item_fields()` |
| `mark_fired_items(fired_items, fired_mask)` | fired item、driver 返回 mask | 对已被 DUT 接收的 item 做 `mark_issue_fire` | 根据 target 把 uop_index 映射到 port 0..6，只标记真正 fired 的 port | `issue_sched.mark_issue_fire()`、`mark_issue_fire_already_accepted()`、`submit_issue_accept_pass()` |
| `item_needs_issue_accept_pass(item)` | item | bit | 在真实 STD writeback pass 关闭时，为普通 store STD 构造软件 pass | `get_std_real_wb_pass_en()`、`data.get_main_transaction()`、`is_store_fuoptype()` |
| `make_issue_accept_pass_event(item)` | item | `memblock_wb_event_t` | 构造 STD_FEEDBACK pass event，模拟 STD accept 后完成 | `data.get_status()`、`data.make_empty_wb_event()`、`status.get_target_issue_epoch()` |
| `submit_issue_accept_pass(item)` | item | 写回 STD pass 状态 | 如果需要软件 STD pass，则直接交给 writeback handler | `item_needs_issue_accept_pass()`、`make_issue_accept_pass_event()`、`issue_accept_wb_handler.handle_event()` |

### F.2.1 issue/LSQ/L2TLB driver 边界

sequence 只负责生成 xaction，真正和 DUT ready/valid 对齐的是 agent driver。文档实现时需要把 sequence 和 driver 当成两个阶段：sequence 选择候选并填字段；driver 等 DUT ready/canAccept 后才算该 xaction 被接口接受。

| driver 函数 | 输入 | 输出/副作用 | 功能 | 被哪个 sequence 使用 |
|---|---|---|---|---|
| `lsqenq_agent_agent_driver::send_pkt(tr)` | LSQENQ xaction | 驱动 `enqLsq_needAlloc/req/canAccept`，采样 resp | 若 `memblock_dispatch_wait_can_accept=1`，先进入等待 canAccept 流程；发送后采集 DUT 返回的 LQ/SQ key | `memblock_lsqenq_dispatch_base_sequence::send_lsqenq_cycle()` |
| `wait_lsq_can_accept(tr)` | LSQENQ xaction | 等 DUT canAccept；可能置 `memblock_dispatch_aborted_by_redirect=1` | 等待期间持续比较 `dispatch_flush_epoch`，如果 redirect/flush 跨拍发生，则 abort，避免把旧 admission 当成成功 | `send_pkt()` |
| `sample_lsqenq_resp(tr)` | LSQENQ xaction | 回填每个 slot 的 DUT resp LQ/SQ key | sequence 后续用这些 key 调 `commit_allocate_with_resp()`，保证软件 LSQ 分配和 DUT 对齐 | `send_pkt()` |
| `lintsissue_agent_agent_driver::send_pkt(tr)` / `drive_dispatch_issue_one_cycle()` / `wait_dispatch_issue_ready()` | issue xaction | 驱动 7 个 intIssue valid/payload；回填 fired mask | 阻塞模式等待各 pipe ready；非阻塞模式只采样一次 ready。两种模式都只有 valid&&ready 的 port 才置入 `memblock_dispatch_fired_mask`；如果 flush epoch 改变，则 abort 未 fire 的 port | `memblock_issue_dispatch_base_sequence::send_issue_cycle()` |
| `lsqcommit_agent_agent_driver::send_pkt(tr)` | commit xaction | 驱动 pendingPtr/flushSb | commit sequence 已在发送前决定 pendingPtr 或 flushSb，driver 只负责把 xaction 打到接口 | `memblock_lsqcommit_dispatch_base_sequence::send_lsqcommit_cycle()` |
| `redirect_agent_agent_driver::send_pkt(tr)` | redirect xaction | 驱动 redirect valid/payload | redirect sequence 发完后调用 `mark_redirect_drive_done()`，exception handler 下一服务周期才能 apply flush | `memblock_redirect_dispatch_base_sequence::drive_redirect_payload()` |
| `L2tlb_agent_agent_driver::send_pkt(tr)` | L2TLB xaction | 驱动 DTLB/L2TLB request ready 和 response payload | L2TLB responder 用 ready/wait/response 多个 xaction 组合完成一次 request-response | `memblock_l2tlb_base_sequence::send_l2tlb_item()` |

### F.3 字段填充函数表

| 函数 | 输入 | 输出/副作用 | 功能 | 子函数 |
|---|---|---|---|---|
| `issue_field_assigner::clear_lintsissue_xaction(tr)` | xaction | 清所有 issue valid/ready/payload | 每拍发射前初始化 xaction | 无 |
| `assign_issue_item_fields(tr, item, pipe_idx)` | xaction、item、pipe | 填主表字段、依赖字段、后端 meta 字段 | 发射字段总入口 | `assign_main_issue_fields()`、`assign_issue_dep_fields()`、`assign_backend_meta_fields()` |
| `assign_main_issue_fields(tr, item, pipe_idx)` | xaction、item、pipe | 写 fuType/fuOpType/src/imm/ROB/LQ/SQ 等主字段 | 把主表和 issue item 关键字段写到对应 LOAD/STA/STD pipe | `data.get_main_transaction()`、`assign_load_main_fields()`、`assign_sta_main_fields()`、`assign_std_main_fields()` |
| `assign_load_main_fields()` | xaction、main_tr、item、pipe | 写 intIssue 0/1/2 | LOAD pipe 主字段 | 无 |
| `assign_sta_main_fields()` | xaction、main_tr、item、pipe | 写 intIssue 3/4 | STA pipe 主字段 | 无 |
| `assign_std_main_fields()` | xaction、main_tr、item、pipe | 写 intIssue 5/6 | STD pipe 主字段 | 无 |
| `assign_issue_dep_fields(tr, item, pipe_idx)` | xaction、item、pipe | 写 MDP/依赖字段 | 对 load 写 `loadWaitBit/waitForRobIdx/storeSetHit/loadWaitStrict`；对 STA 写 `isFirstIssue/storeSetHit/ssid` | `deterministic_percent_hit()`、`select_prior_store_for_load()`、`data.get_status()` |
| `select_prior_store_for_load(uid, wait_rob_key)` | uid | 更老 store ROB key | 为 load 生成 waitForRobIdx | `data.get_main_transaction()`、`data.get_status()`、`derive_op_behavior()` |
| `assign_backend_meta_fields(tr, item, pipe_idx)` | xaction、item、pipe | 写 `pdest/rfWen/fpWen/pc/isRVC/ftqIdx/ftqOffset` | 这些字段主要随流水到后端；load/AMO 的 rfWen/fpWen/pdest 影响写回目的 | `derive_wen()`、`compute_pdest()`、`compute_pc()`、`compute_is_rvc()`、`compute_ftq_*()` |
| `derive_wen(main_tr, rfWen, fpWen)` | 主表 | rfWen/fpWen | INT load/AMO -> rfWen，FP load -> fpWen，二者互斥 | 无 |
| `compute_pdest(uid, has_wb)` | uid、是否写回 | pdest | 生成写回物理寄存器号；无写回则 0 | `get_pdest_base()`、`get_pdest_range()` |
| `compute_pc(uid)` | uid | pc | 按 `pc_base + uid * pc_stride` 生成 PC | `get_pc_base()`、`get_pc_stride()` |
| `compute_is_rvc(uid)` | uid | isRVC | 按权重生成压缩指令标志 | `deterministic_percent_hit()`、`get_rvc_wt()` |
| `compute_ftq_flag/value/offset(uid)` | uid | ftq 字段 | 生成 FTQ metadata | `get_ftq_idx_base()` |
| `deterministic_percent_hit(uid, salt, percent)` | uid、salt、百分比 | bit | 稳定的伪随机百分比命中，避免同一 uid 多次调用结果漂移 | 无 |
| `check_pipe_idx(target, pipe_idx, caller)` | target、pipe、caller | 非法 fatal | 限制 LOAD<3、STA<2、STD<2 | `is_valid_pipe_idx()` |

## 8. 阶段 G：monitor raw event 采集与 writeback 状态更新

### G.1 raw monitor 采集

monitor 负责采集 DUT 事实并写入 `memblock_sync_pkg` raw queue：

```text
io_mem_to_ooo_int_wb_agent_monitor
  -> 构造 dispatch_raw_int_wb_t
  -> memblock_sync_pkg::push_raw_int_wb(raw)

io_mem_to_ooo_iq_feedback_agent_monitor
  -> 构造 dispatch_raw_iq_feedback_t
  -> memblock_sync_pkg::push_raw_iq_feedback(raw)

io_mem_to_ooo_ctrl_agent_monitor
  -> lqDeq/sqDeq/memoryViolation/sbIsEmpty
  -> 构造 dispatch_raw_ctrl_t
  -> memblock_sync_pkg::push_raw_ctrl(raw)

csr_ctrl_agent_monitor
  -> 采 CSR runtime 字段
  -> raw_csr_payload_changed(last, cur) 时 push
  -> memblock_sync_pkg::push_raw_csr(raw)
```

raw queue push 函数都受 `dispatch_monitor_capture_en` 控制，测试结束 `end_test_check()` 会关掉 capture 并清空残留 raw event。

### G.1.1 monitor 边界函数

| monitor 函数 | 输入 | 输出/副作用 | 功能 | 后续消费者 |
|---|---|---|---|---|
| `<agent>_monitor::run_phase(phase)` | UVM phase | 常驻调用 `mon_data()` | monitor 主循环入口 | agent analysis port / raw queue |
| `io_mem_to_ooo_int_wb_agent_monitor::mon_data()` | DUT int writeback 接口 | `push_raw_int_wb(raw)` | 采 load/STA/STD writeback port、ROB/LQ/SQ key、exception vec，形成普通 pass/fault 原始事实 | `dispatch_monitor_event_adapter::drain_writeback_events()` |
| `io_mem_to_ooo_iq_feedback_agent_monitor::mon_data()` | DUT IQ feedback 接口 | `push_raw_iq_feedback(raw)` | 采 STA/STD feedback；`hit=1` 表示 pass，`hit=0` 表示 backend replay，`flush_state` 可转换成 PTW-back replay | `convert_raw_iq_feedback()` |
| `io_mem_to_ooo_ctrl_agent_monitor::mon_data()` | DUT ctrl 接口 | `push_raw_ctrl(raw)` | 采 `lqDeq/sqDeq/memoryViolation/sbIsEmpty`；其中 deq 直接释放 LSQ map，memoryViolation 转 redirect event | `drain_exception_and_redirect_events()` |
| `csr_ctrl_agent_monitor::mon_data()` | CSR runtime 接口 | `push_raw_csr(raw)` | 采真实运行时 CSR 镜像；payload 变化时覆盖 latest CSR snapshot | `drain_csr_events()`、L2TLB lookup |
| `L2tlb_agent_agent_monitor::mon_data()` | DTLB/L2TLB 接口 | analysis port 输出 xaction | 当前 L2TLB responder 主要直接读 vif request；monitor 可用于常规 agent 观测和 debug | env/RM/scb 或后续扩展 |

### G.2 monitor adapter 调用链

```text
memblock_main_dispatch_auto_build_main_table_base_sequence::service_monitor_once()
  -> collect_runtime_context_events()
       -> monitor_adapter.drain_csr_events()
       -> monitor_adapter.drain_sfence_events()
            -> pop_raw_sfence()
            -> data.apply_raw_sfence()
  -> collect_writeback_events()
       -> monitor_adapter.drain_writeback_events(writeback_handler)
            -> drain_csr_events()
            -> pop_raw_int_wb()
               -> convert_raw_int_wb()
               -> writeback_handler.handle_event()
            -> pop_raw_iq_feedback()
               -> convert_raw_iq_feedback()
               -> writeback_handler.handle_event()
  -> collect_exception_and_redirect_events()
       -> monitor_adapter.drain_exception_and_redirect_events(writeback_handler)
            -> drain_csr_events()
            -> pop_raw_ctrl()
               -> apply_raw_ctrl_deq()
               -> convert_raw_memory_violation()
               -> writeback_handler.handle_event()
  -> exception_redirect_replay_task()
       -> exception_handler.process_pending_events()
```

### G.3 `memblock_sync_pkg` 函数表

| 函数 | 输入 | 输出/副作用 | 功能 |
|---|---|---|---|
| `make_empty_raw_int_wb()` | 无 | 空 int writeback raw | 初始化 LOAD/STORE writeback raw event |
| `make_empty_raw_iq_feedback()` | 无 | 空 IQ feedback raw | 初始化 STA/STD feedback raw event |
| `make_empty_raw_ctrl()` | 无 | 空 ctrl raw | 初始化 LQ/SQ deq、memoryViolation、sbIsEmpty raw event |
| `make_empty_raw_csr()` | 无 | 空 CSR raw | 初始化 runtime CSR raw event |
| `raw_csr_payload_changed(prev, cur)` | 两个 raw CSR | bit | 判断 CSR payload 是否变化，减少重复 push |
| `push_raw_int_wb(item)` | raw int wb | 入 `raw_int_wb_q` | capture 开启且 valid 时保存 monitor 事实 |
| `pop_raw_int_wb(item)` | output | 出队一个 int wb raw | adapter 消费 raw event |
| `push_raw_iq_feedback(item)` / `pop_raw_iq_feedback(item)` | raw IQ | push/pop | IQ feedback raw queue |
| `push_raw_ctrl(item)` / `pop_raw_ctrl(item)` | raw ctrl | push/pop | ctrl raw queue |
| `push_raw_sfence(item)` / `pop_raw_sfence(item)` | raw sfence/hfence | push/pop | sfence/hfence 是离散 TLB invalidation 事件，必须 FIFO 消费，不能像 CSR snapshot 一样覆盖成 latest |
| `push_raw_csr(item)` / `get_latest_raw_csr(item, seq)` | raw CSR | latest snapshot / seq | CSR runtime latest snapshot |
| `clear_raw_monitor_queues()` | 无 | 清 raw queues，service cycle 清 0 | test reset/end 清理 |
| `tick_dispatch_service_cycle()` | 无 | `dispatch_service_cycle++` | 给 TB 软件服务循环提供周期号 |
| `get_dispatch_service_cycle()` | 无 | cycle | replay wait/redirect timeout/flushSb timeout 使用 |
| `raw_monitor_queue_size()` | 无 | raw queue 总长度 | end check 判断残留 |

### G.4 `dispatch_monitor_event_adapter` 函数表

| 函数 | 输入 | 输出/副作用 | 功能 | 子函数 |
|---|---|---|---|---|
| `bind_commit_handler(handler)` | `lsq_commit_handler` | 保存句柄 | ctrl deq 需要回调 commit handler | 无 |
| `ensure_handles()` | 无 | 获取 data/commit handler | adapter 入口保护 | `common_data_transaction::get()`、`type_id::create()` |
| `make_wb_event_base()` | 无 | 空 `memblock_wb_event_t` | 转换 raw event 的基础对象 | `data.make_empty_wb_event()` |
| `raw_rob_to_key(valid, flag, value, key)` | raw ROB | key + valid | 转换 raw ROB 字段 | 无 |
| `raw_lq_to_key(valid, flag, value, key)` | raw LQ | key + valid | 转换 raw LQ 字段 | 无 |
| `raw_sq_to_key(valid, flag, value, key)` | raw SQ | key + valid | 转换 raw SQ 字段 | 无 |
| `event_has_active_uid(wb_event)` | wb event | bit | 用 active ROB/LQ/SQ map 确认 event 能归属 uid | `data.resolve_uid_for_event()` |
| `convert_raw_int_wb(raw, wb_event)` | raw int wb | wb event + bit | 把 load/store writeback port 转成 target/source，并过滤无 active uid 的 event | `make_wb_event_base()`、`raw_*_to_key()`、`event_has_active_uid()` |
| `convert_raw_iq_feedback(raw, wb_event)` | raw IQ feedback | wb event + bit | 把 STA/STD feedback 转成 pass/replay event；`hit=1` pass，`hit=0` replay | `make_wb_event_base()`、`raw_*_to_key()`、`event_has_active_uid()` |
| `convert_raw_memory_violation(raw, wb_event)` | raw ctrl | redirect wb event + bit | 把 memoryViolation 转成 redirect event | `make_wb_event_base()`、`raw_rob_to_key()`、`event_has_active_uid()` |
| `apply_raw_ctrl_deq(raw)` | raw ctrl | 更新 sbIsEmpty、LQ/SQ deq | ctrl raw 中 deq 不走 wb_event，而是直接交 commit handler 释放资源 | `data.update_sb_is_empty()`、`monitor_commit_handler.apply_raw_ctrl_deq()` |
| `drain_csr_events()` | 无 | 更新 runtime CSR | 读取 latest CSR snapshot 和 seq；公共数据侧按 seq 去重 | `get_latest_raw_csr()`、`data.apply_raw_csr_runtime()` |
| `drain_sfence_events()` | 无 | 删除命中的 live TLB entry | FIFO 消费 raw sfence/hfence queue；每条 fence event 交给公共数据层执行 entry 级失效 | `pop_raw_sfence()`、`data.apply_raw_sfence()` |
| `drain_writeback_events(writeback_handler)` | writeback handler | 正常 wb/IQ feedback 状态更新或事件入队 | 消费 int wb 和 IQ feedback raw queue | `drain_csr_events()`、`pop_raw_int_wb()`、`convert_raw_int_wb()`、`writeback_handler.handle_event()`、`pop_raw_iq_feedback()`、`convert_raw_iq_feedback()` |
| `drain_exception_and_redirect_events(writeback_handler)` | writeback handler | ctrl deq、memoryViolation redirect 处理 | 消费 ctrl raw queue | `drain_csr_events()`、`pop_raw_ctrl()`、`apply_raw_ctrl_deq()`、`convert_raw_memory_violation()`、`writeback_handler.handle_event()` |

### G.5 `writeback_status_handler` 函数表

| 函数 | 输入 | 输出/副作用 | 功能 | 子函数 |
|---|---|---|---|---|
| `event_has_fault(wb_event)` | wb event | bit | 判断 exception_vec 或 has_exception | 无 |
| `event_is_redirect(wb_event)` | wb event | bit | 判断 redirect event | 无 |
| `event_is_replay(wb_event)` | wb event | bit | 判断 replay event | 无 |
| `event_is_normal_pass(wb_event)` | wb event | bit | writeback valid 且非 redirect/replay/fault | `event_is_redirect()`、`event_is_replay()`、`event_has_fault()` |
| `validate_event_target(wb_event, caller)` | event、caller | 非法 target fatal | 非 redirect event 必须是 LOAD/STA/STD | 无 |
| `handle_event(wb_event)` | wb event | 更新 status 或 push feedback queue | 所有 writeback/replay/redirect/fault event 的统一入口 | `data.normalize_feedback_event()`、`event_is_redirect()`、`event_is_replay()`、`event_has_fault()`、`data.mark_target_fault()`、`data.push_feedback_event()`、`data.mark_target_normal_pass()` |
writeback 关键判断：

- 正常 pass：`real_wb_valid=1`，且不是 replay/redirect/fault。调用 `data.mark_target_normal_pass()`。
- fault：调用 `data.mark_target_fault()` 并 push 到 `exception_event_q`，后续 exception handler 继续处理。
- replay：先 push 到 `exception_event_q`，后续 exception handler 置 `replay_pending`。
- redirect：先 push 到 `exception_event_q`，后续 exception handler 选择最老 redirect 并发起 flush。

## 9. 阶段 H：ROB commit 与 LQ/SQ deq

commit 是 TB 主动驱动 backend->MemBlock 的 pendingPtr；deq 是 DUT ctrl monitor 采回的真实 LQ/SQ 释放事实。两者都完成后 uid 才能 retire。

### H.1 commit 调用链

```text
memblock_lsqcommit_dispatch_base_sequence::body()
  -> seq_csr_common::init()
  -> configure_from_plus()
  -> if !enable: super.body(); return
  -> ensure_helpers()
  -> wait_for_main_table()
  -> drive_lsqcommit_loop()

drive_lsqcommit_loop()
  loop:
    has_lsqcommit_activity()
    send_lsqcommit_cycle(cycle_idx, has_commit)

send_lsqcommit_cycle(cycle_idx, has_commit)
  -> data.request_scheduled_flushsb_if_due(cycle_idx)
  -> if issue_blocked_by_global_flush():
       idle xaction
       return
  -> drive_flushsb_if_needed(cycle_idx, did_flushsb_drive)
  -> if did_flushsb_drive or waiting empty:
       idle xaction
       return
  -> commit_handler.build_lsqcommit_xaction(tr, commit_uids, has_commit)
       -> select_rob_commit_batch()
       -> clear_lsqcommit_xaction()
       -> if has_commit: pendingPtr = last uid rob_key
  -> start_item(tr); finish_item(tr)
  -> if has_commit:
       commit_handler.mark_rob_commit_batch(commit_uids)
```

### H.2 deq 调用链

```text
io_mem_to_ooo_ctrl monitor
  -> memblock_sync_pkg::push_raw_ctrl(raw)

service_monitor_once()
  -> collect_exception_and_redirect_events()
       -> monitor_adapter.drain_exception_and_redirect_events()
            -> pop_raw_ctrl(raw)
            -> monitor_adapter.apply_raw_ctrl_deq(raw)
                 -> data.update_sb_is_empty(raw.sb_is_empty)
                 -> monitor_commit_handler.apply_raw_ctrl_deq(lq_count, lq_ptr, sq_count, sq_ptr)
                      -> apply_dut_lq_deq()
                           -> lq_deq_start_key()
                           -> data.lookup_active_uid_by_lq()
                           -> lsq_ctrl.release_lq(count)
                           -> data.release_uid_lq_mapping(uid)
                           -> data.try_retire_committed_uid(uid)
                      -> apply_dut_sq_deq()
                           -> sq_deq_start_key()
                           -> data.lookup_active_uid_by_sq()
                           -> lsq_ctrl.release_sq(count)
                           -> data.release_uid_sq_mapping(uid)
                           -> data.try_retire_committed_uid(uid)
```

### H.3 commit/deq 函数表

| 函数 | 输入 | 输出/副作用 | 功能 | 子函数 |
|---|---|---|---|---|
| `lsq_commit_handler::bind_lsq_ctrl(ctrl)` | LSQ model | 保存句柄 | deq 释放资源需要软件 LSQ 模型 | 无 |
| `ensure_handles()` | 无 | 获取 data/lsq_ctrl | 入口保护 | `common_data_transaction::get()`、`lsq_ctrl_model::get()` |
| `report_deq_mismatch(msg)` | 信息 | warning 或 fatal | DUT deq 与软件 head 不一致时统一处理 | `get_lsq_resync_on_mismatch()` |
| `uid_is_commit_candidate(uid)` | uid | bit | 判断 uid 是否能驱动 ROB commit：active/writeback/pass/required target done/无异常 pending | `data.issue_blocked_by_global_flush()`、`data.get_status()`、`data.required_targets_done()` |
| `advance_commit_cursor_past_done()` | 无 | 推进 `commit_cursor_uid` | 跳过已 commit/success/flushed 的 uid | `data.get_status()` |
| `select_rob_commit_batch(uids)` | output uid 队列 | 最多 `MEMBLOCK_COMMIT_WIDTH` 个连续可 commit uid | 按 uid 顺序选择 commit batch，遇到第一个不可 commit 停止 | `advance_commit_cursor_past_done()`、`uid_is_commit_candidate()` |
| `clear_lsqcommit_xaction(tr)` | xaction | pendingPtr 保持 last，flushSb=0 | idle/默认 commit xaction 初始化 | 无 |
| `build_lsqcommit_xaction(tr, commit_uids, has_commit)` | output | 创建 commit xaction | 用最后一个 commit uid 的 ROB key 填 pendingPtr | `select_rob_commit_batch()`、`clear_lsqcommit_xaction()`、`data.get_status().get_rob_key()` |
| `mark_rob_commit_uid(uid)` | uid | `rob_commit=1`，可能置 `lsq_deq=1` 并 retire | commit xaction 发送后更新软件状态 | `uid_is_commit_candidate()`、`data.try_retire_committed_uid()`、`advance_commit_cursor_past_done()` |
| `mark_rob_commit_batch(uids)` | uid 队列 | 批量 commit | 遍历调用单 uid commit | `mark_rob_commit_uid()` |
| `lq_deq_start_key(deq_ptr, count, ptr_is_next)` | DUT deq ptr/count | 起始 LQ key | DUT ptr 为 next ptr 时回退 count 得到出队起点 | `lsq_ctrl_model::rewind_lq_key()` |
| `sq_deq_start_key(deq_ptr, count, ptr_is_next)` | DUT deq ptr/count | 起始 SQ key | 同上，用于 SQ | `rewind_sq_key()` |
| `apply_dut_lq_deq(count, deq_ptr, ptr_is_next)` | count、DUT LQ ptr | 释放 LQ 资源和 active map | 校验 DUT deq 起点等于软件 head，逐项查 uid，然后释放 | `lq_deq_start_key()`、`data.lookup_active_uid_by_lq()`、`report_deq_mismatch()`、`lsq_ctrl.release_lq()`、`data.release_uid_lq_mapping()`、`data.try_retire_committed_uid()` |
| `apply_dut_sq_deq(count, deq_ptr, ptr_is_next)` | count、DUT SQ ptr | 释放 SQ 资源和 active map | SQ 版本 deq 处理 | `sq_deq_start_key()`、`data.lookup_active_uid_by_sq()`、`report_deq_mismatch()`、`lsq_ctrl.release_sq()`、`data.release_uid_sq_mapping()`、`data.try_retire_committed_uid()` |
| `apply_raw_ctrl_deq(lq_count, lq_ptr, sq_count, sq_ptr, ptr_is_next)` | raw ctrl deq 字段 | LQ/SQ deq 更新 | 当前唯一 ctrl deq 入口，raw adapter 调用 | `apply_dut_lq_deq()`、`apply_dut_sq_deq()` |
| `memblock_lsqcommit_dispatch_base_sequence::has_lsqcommit_activity()` | 无 | bit | 判断 commit sequence 是否应该开始/继续：有 flushSb pending、scheduled flushSb、commit candidate 或已 commit 记录都算 activity | `commit_handler.uid_is_commit_candidate()`、`data.get_status()` |
| `flushsb_request_pending(cycle_idx)` | cycle | bit | 判断当前或未来是否还有 flushSb 请求未完成，避免 commit loop 因 idle_stop 过早退出 | `data.scheduled_flushsb_pending()` |
| `memblock_lsqcommit_dispatch_base_sequence::drive_flushsb_if_needed(cycle_idx, did_drive)` | cycle | 可能驱动 `flushSb=1` | 按调度周期发 flushSb，并等待 sbIsEmpty 由 ctrl monitor 清除 waiting | `data.flushsb_timed_out()`、`data.should_drive_flushsb()`、`commit_handler.clear_lsqcommit_xaction()`、`data.mark_flushsb_driven()` |

commit 与 deq 的关系：

- `rob_commit=1` 表示 TB 已经按 ROB 顺序把 pendingPtr 推过该 uid。
- `lsq_deq=1` 表示该 uid 的 active LQ/SQ 映射已全部释放。
- `try_retire_committed_uid()` 要求 `rob_commit=1` 且 `active_lq_mapped=0` 且 `active_sq_mapped=0`，然后计算 `success` 并调用 `retire_active_uid()`。
- 如果 ROB commit 时 LQ/SQ 已经释放，`mark_rob_commit_uid()` 会直接置 `lsq_deq=1`；如果仍有 active 映射，则等 DUT ctrl deq 触发 `release_uid_lq_mapping()` 或 `release_uid_sq_mapping()` 后再置。

## 10. 阶段 I：异常、redirect、backend replay

### I.1 复杂事件进入队列

```text
writeback_status_handler::handle_event(wb_event)
  -> data.normalize_feedback_event()
  -> if redirect:
       data.push_feedback_event(wb_event)
  -> else if replay:
       data.push_feedback_event(wb_event)
  -> else if fault:
       data.mark_target_fault()
       data.push_feedback_event(wb_event)
  -> else if normal pass:
       data.mark_target_normal_pass()
```

### I.2 exception/replay/redirect 主调用链

```text
exception_redirect_replay_handler::process_pending_events()
  -> service_ptw_wait_replay()
       -> data.pop_ready_ptw_wait_replay(timeout, wait_item, timed_out)
       -> data.mark_replay_pending(wait_item.uid, ...)
  -> advance_active_redirect()
       -> if data.redirect_drive_done_for(active_redirect):
            data.apply_redirect_flush(active_redirect)
       -> else timeout fatal
  -> if active_redirect.valid: return
  -> while data.pop_feedback_event(wb_event):
       events.push_back(wb_event)
  -> if select_oldest_redirect(events, redirect_event):
       redirect = redirect_from_event(redirect_event)
       data.request_redirect_flush(redirect)
       data.push_redirect_drive(redirect)
       requeue_non_redirect_events(events)
       return
  -> foreach events:
       if replay: handle_replay_event(event)
       else if fault: handle_fault_event(event)
```

redirect 期间：

```text
data.request_redirect_flush(redirect)
  -> redirect_phase = DETECTED / FREEZE_REQUESTED
  -> flush_in_progress = 1
  -> memblock_sync_pkg::dispatch_flush_in_progress = 1
  -> memblock_sync_pkg::dispatch_flush_epoch++
  -> issue_freeze_ack = 1
  -> active_redirect = redirect

memblock_redirect_dispatch_base_sequence::body()
  -> data.try_pop_redirect_drive(payload)
  -> drive_redirect_payload(payload)
       -> assign_redirect_xaction()
       -> start_item(); finish_item()
       -> data.mark_redirect_drive_done(payload)

exception_redirect_replay_handler::advance_active_redirect()
  -> data.redirect_drive_done_for(redirect)
  -> data.apply_redirect_flush(redirect)
       -> foreach active uid:
            if rob_need_flush(status.rob_key, redirect):
              redirect_pending=1
              flushed=1
              clear_uid_dispatch_result()
              retire_active_uid()
       -> clear_ptw_wait_replay_by_redirect()
       -> clear_redirect_drive_queue()
       -> flush_in_progress=0
       -> dispatch_flush_in_progress=0
       -> issue_freeze_ack=0
       -> active_redirect=0
```

backend replay 期间：

```text
handle_replay_event(wb_event)
  -> data.resolve_uid_for_event(wb_event, uid)
  -> issue_epoch = data.get_event_issue_epoch(wb_event, uid)
  -> replay_seq  = data.get_event_replay_seq(wb_event, uid)
  -> if event_should_wait_ptw(wb_event):
       data.push_ptw_wait_replay(uid, target, issue_epoch, replay_seq, cycle)
       return
  -> data.mark_replay_pending(uid, target, issue_epoch, replay_seq, cycle)
       -> remove_uid_from_issue_queues(uid)
       -> replay_pending=1
       -> clear writeback/pass/success
       -> clear target dispatched/writeback/pass
       -> set replay_target_<target>=1
       -> bump_replay_seq(uid)

后续 service/issue loop:
  route_all_ready_uids()
    -> route_uid(uid)
       -> status.replay_pending=1 且 replay_target_requested(target)=1
       -> route_target()
          -> delete old queue entry
          -> make_issue_item() 使用新的 status.replay_seq
          -> push_issue_queue_item()
  issue fire:
    -> mark_issue_fire()
       -> mark_issue_snapshot(new issue_epoch)
       -> delete_issue_queue_entry(target, uid, replay_seq, match=1)
       -> clear_replay_target_after_fire()
          -> 如果所有 replay target 清空，则 replay_pending=0
```

### I.3 exception/replay/redirect 函数表

| 函数 | 输入 | 输出/副作用 | 功能 | 子函数 |
|---|---|---|---|---|
| `event_is_redirect()` / `event_is_replay()` / `event_is_fault()` | wb event | bit | 事件分类 | 无 |
| `event_should_wait_ptw(wb_event)` | wb event | bit | PTW-back replay 是否先等待 TLB response | `get_replay_wait_ptw_en()` |
| `redirect_from_event(wb_event)` | wb event | redirect payload | 从 canonical `redirect.valid` 的 event 提取 redirect payload；`redirect_valid` 不一致会在分类 helper fatal | 无 |
| `redirect_event_is_older(candidate, best)` | 两个 redirect event | bit | 按 ROB 年龄和 port_id 选最老 redirect | `redirect_from_event()`、`rob_order_util::rob_is_after()` |
| `select_oldest_redirect(events, selected)` | event 队列 | selected redirect event | redirect 优先级高于 replay/fault，且只处理最老 redirect | `event_is_redirect()`、`redirect_event_is_older()` |
| `handle_replay_event(wb_event)` | replay event | 置 replay pending 或加入 PTW wait queue | backend replay 入口 | `data.resolve_uid_for_event()`、`data.get_event_issue_epoch()`、`data.get_event_replay_seq()`、`event_should_wait_ptw()`、`data.push_ptw_wait_replay()`、`data.mark_replay_pending()` |
| `handle_fault_event(wb_event)` | fault event | 消费 recovery 队列事件，不写 fault 状态 | fault 事件兜底处理；target fault 已由 writeback handler 首次落表 | `data.resolve_uid_for_event()`、`data.get_event_issue_epoch()`、`data.get_event_replay_seq()` |
| `service_ptw_wait_replay()` | 无 | 到期或 TLB ready 的 wait replay 转成 replay_pending | 处理 PTW-back replay 延迟 | `data.pop_ready_ptw_wait_replay()`、`data.mark_replay_pending()` |
| `requeue_non_redirect_events(events)` | event 队列 | 非 redirect 事件放回队首 | redirect recovery 期间暂停 replay/fault 处理，避免状态冲突 | `data.exception_event_q.push_front()` |
| `advance_active_redirect()` | 无 | 可能 apply redirect flush | redirect 已驱动后推进软件 flush；超时 fatal | `data.redirect_drive_done_for()`、`data.apply_redirect_flush()`、`get_redirect_freeze_timeout()` |
| `process_pending_events()` | 无 | replay/redirect/fault 状态推进 | 复杂事件主入口 | `service_ptw_wait_replay()`、`advance_active_redirect()`、`data.pop_feedback_event()`、`select_oldest_redirect()`、`data.request_redirect_flush()`、`data.push_redirect_drive()`、`requeue_non_redirect_events()`、`handle_replay_event()`、`handle_fault_event()` |

### I.4 replay 与 redirect 关键状态函数

| 函数 | 输入 | 输出/副作用 | 功能 |
|---|---|---|---|
| `data.mark_replay_pending(uid, target, issue_epoch, replay_seq, cycle)` | uid、target、旧 issue epoch、旧 replay_seq | 清旧队列，置 replay pending，target dispatched/pass 清 0，`replay_seq++` | backend replay 的核心状态切换；旧反馈通过 epoch/replay_seq 过滤 |
| `data.clear_replay_target_after_fire(uid, target)` | uid、target | 清 replay target bit；所有 target 清完则 `replay_pending=0` | replay item 重新发射成功后退出 pending |
| `data.push_ptw_wait_replay(uid, target, issue_epoch, replay_seq, start_cycle)` | replay 信息 | 入 `ptw_wait_replay_q` | PTW-back replay 等待 TLB 表 response 或超时 |
| `data.pop_ready_ptw_wait_replay(timeout, wait_item, timed_out)` | timeout | ready item | TLB response done 或等待超时后释放 replay |
| `data.request_redirect_flush(redirect)` | redirect payload | 全局冻结、flush epoch++、active_redirect 设置 | redirect recovery 开始，禁止 admission/issue/commit |
| `data.push_redirect_drive(payload)` | redirect payload | 入 redirect drive queue | 给 `memblock_redirect_dispatch_base_sequence` 驱动真实 redirect 口 |
| `data.try_pop_redirect_drive(payload)` | output payload | 从 redirect drive queue 取一个并标记 inflight | redirect sequence 使用 |
| `data.mark_redirect_drive_done(payload)` | payload | inflight 清 0，drive_done_epoch++ | redirect 口已驱动完成 |
| `data.redirect_drive_done_for(payload)` | payload | bit | 判断 payload 已驱动且跨过一个 service cycle，可 apply flush | 无 |
| `data.apply_redirect_flush(redirect)` | redirect payload | flush 对应 uid，清 recovery 状态 | 按 ROB 顺序 flush `rob_need_flush()` 命中的 active uid | `rob_order_util::rob_need_flush()`、`clear_uid_dispatch_result()`、`retire_active_uid()`、`clear_ptw_wait_replay_by_redirect()` |

## 11. 阶段 J：状态管理与 final check

### J.1 `common_data_transaction` 核心调用关系

```text
reset_all_tables(N)
  -> clear raw queue / issue queue / feedback queue / redirect queue / wait replay queue
  -> new main_table/status/tlb arrays
  -> status.reset(uid)

alloc_uid()
  -> uid = next_uid++

set_main_transaction(uid, tr)
  -> main_table_by_uid[uid] = tr

init_status_for_uid(uid)
  -> status.reset(uid)
  -> if main exists: snapshot_from_main()

activate_uid(uid, map_lq, map_sq)
  -> status.snapshot_from_main(main_tr)
  -> rob_to_map_key(main_tr.get_rob_key())
  -> uid_by_active_rob[key] = uid
  -> if map_lq: uid_by_lq[lq_key] = uid
  -> if map_sq: uid_by_sq[sq_key] = uid
  -> status.active = 1

normal pass:
  mark_target_normal_pass()
    -> conditional_set_target_status_field(writeback/pass)
    -> if required_targets_done(): status.writeback=1; status.pass=1

fault:
  mark_target_fault()
    -> target_writeback=1; target_fault=1
    -> fault=1; exception_pending=1

commit/deq:
  mark_rob_commit_uid()
  release_uid_lq_mapping()
  release_uid_sq_mapping()
  try_retire_committed_uid()
    -> retire_active_uid()

end_test_check()
  -> 关闭 monitor capture
  -> raw queue 清空
  -> 所有 uid inactive 且无 pending
  -> active ROB/LQ/SQ map 空
  -> issue queue 空
  -> redirect/flush/flushSb/PTW wait 状态 idle
```

### J.2 `common_data_transaction` 函数表

| 函数 | 输入 | 输出/副作用 | 功能 | 子函数 |
|---|---|---|---|---|
| `get()` | 无 | 单例 | 公共数据 owner 获取入口 | `new()` |
| `reset_all_tables(main_trans_num_i)` | 主表行数 | 清所有表、队列、全局同步状态，重建 status 数组 | 每个 testcase/main table 开始的硬 reset | `memblock_sync_pkg::clear_raw_monitor_queues()`、`clear_issue_queues()`、`clear_feedback_events()`、`clear_redirect_drive_queue()`、`clear_ptw_wait_replay_queue()`、`mmu_csr_state.reset()`、`status.reset()` |
| `alloc_uid()` | 无 | uid | 连续分配 uid | 无 |
| `check_uid(uid, caller)` | uid、caller | 非法 fatal | 保护所有数组访问 | `is_valid_uid()` |
| `set_main_transaction(uid, tr)` | uid、主表 | 写主表 | 主表落地，不允许覆盖 active uid | `check_uid()` |
| `get_main_transaction(uid)` | uid | 主表 transaction | 读取主表 | `check_uid()` |
| `init_status_for_uid(uid)` | uid | status | 初始化 status 并 snapshot 主表 key | `status.reset()`、`status.snapshot_from_main()` |
| `get_status(uid)` | uid | status | 读取状态表 | `check_uid()` |
| `set_status_field(uid, field, value)` / `get_status_field(uid, field)` | uid、枚举字段 | 修改/读取状态位 | 状态字段统一入口 | `ensure_status_exists()`、`get_status()` |
| `conditional_set_target_status_field()` | uid、field、target、issue_epoch、replay_seq | bit | 只有 active、未 killed、target epoch/seq 匹配时才更新 | `status.get_target_issue_epoch()`、`set_status_field()` |
| `alloc_issue_epoch()` | 无 | epoch | 每次 fire 分配全局 issue epoch | 无 |
| `mark_issue_snapshot(uid, target, issue_epoch)` | uid、target、epoch | 写当前 target issue epoch | 后续 writeback/replay 用于过滤 stale event | `status.set_target_issue_epoch()` |
| `target_writeback_field()` / `target_pass_field()` / `target_fault_field()` | target | status enum | target 到状态字段的映射 | 无 |
| `target_entry_done(status, target)` | status、target | bit | 判断 target 是否 pass/fault 完成 | 无 |
| `required_targets_done(uid)` | uid | bit | load 需要 LOAD done；store/atomic 需要 STA 和 STD done | `get_status()`、`get_main_transaction()`、`target_entry_done()` |
| `conditional_set_target_status_field()` | uid、field、target、epoch、seq | bit | 按 target issue epoch 和 replay seq 过滤 stale event | `status.get_target_issue_epoch()`、`set_status_field()` |
| `mark_target_normal_pass()` | uid、target、epoch、seq、cycle | bit | 标记 target writeback/pass；所有 required target done 后置全局 writeback/pass | `conditional_set_target_status_field()`、`target_entry_done()`、`required_targets_done()` |
| `mark_target_fault()` | uid、target、epoch、seq、exception_vec、cycle | bit | 标记 target fault 和全局 fault/exception_pending | `conditional_set_target_status_field()` |
| `set_replay_target_mask(status, replay_load, replay_sta, replay_std)` | status、三个 target bit | 修改 status replay target mask | replay 目标位的统一赋值 helper；当前主路径多在 `mark_replay_pending()` 内按 target 设置，后续多 target replay 可复用 | 无 |
| `resolve_uid_for_event(wb_event, uid)` | wb event | uid | 按 uid/ROB/LQ/SQ active map 反查 uid，并检查多 key 一致性 | `lookup_active_uid_by_rob()`、`lookup_active_uid_by_lq()`、`lookup_active_uid_by_sq()` |
| `normalize_feedback_event(wb_event, normalized_event)` | wb event | 规范化 event | 填 uid/ROB/issue_epoch/replay_seq，过滤无 active uid 或缺 snapshot 的 stale event | `resolve_uid_for_event()`、`get_status()`、`get_target_issue_epoch()` |
| `push_feedback_event()` / `pop_feedback_event()` / `clear_feedback_events()` | wb event | queue push/pop/clear | 复杂 event 队列 | `normalize_feedback_event()` |
| `activate_uid(uid, map_lq, map_sq)` | uid、是否映射 LQ/SQ | active maps 和 status.active | admission 成功后把 uid 纳入运行时管理 | `get_main_transaction()`、`ensure_status_exists()`、`snapshot_from_main()`、`rob_to_map_key()`、`lq_to_map_key()`、`sq_to_map_key()` |
| `activate_uid_by_behavior(uid, behavior)` | uid、behavior | active maps 和 status.active | 按 `behavior.uses_lq/uses_sq` 调 `activate_uid()`，避免调用方手动判断 LQ/SQ 映射 | `activate_uid()` |
| `lookup_active_uid_by_rob/lq/sq()` | key | uid + bit | monitor event 或 deq 反查 uid | `rob_to_map_key()`、`lq_to_map_key()`、`sq_to_map_key()` |
| `get_active_uid_by_rob(rob_key)` | ROB key | uid；不存在 fatal | 需要强制存在 active ROB mapping 的场景使用；普通 monitor event 更适合用 `lookup_active_uid_by_rob()` | `lookup_active_uid_by_rob()` |
| `retire_active_uid(uid)` | uid | 删除 active ROB/LQ/SQ map，`active=0` | uid 生命周期结束清理 | `remove_uid_from_issue_queues()`、`rob_to_map_key()`、`lq_to_map_key()`、`sq_to_map_key()` |
| `release_uid_lq_mapping(uid)` / `release_uid_sq_mapping(uid)` | uid | 删除 LQ/SQ map，更新 `lsq_deq` | DUT deq 后释放 active LSQ 映射 | `lq_to_map_key()`、`sq_to_map_key()` |
| `try_retire_committed_uid(uid)` | uid | 可能 `success=1` 并 retire | ROB commit 且 LQ/SQ map 全释放后完成 uid | `retire_active_uid()` |
| `make_tlb_key_by_req(vpn, s2xlate)` | request vpn/s2xlate | `{vpn,asid,vmid,s2xlate}` key | 用 runtime CSR 生成 by-key TLB 索引 | `mmu_csr_state.make_lookup_key()` |
| `get_or_create_tlb_entry_by_req(vpn, s2xlate, key, entry, created)` | request vpn/s2xlate | key、entry、created | L2TLB responder 查/建 live TLB entry；miss 自动创建 entry | `make_tlb_key_by_req()`、`build_tlb_entry_for_key()`、`insert_tlb_entry()` |
| `register_uid_tlb_record_on_issue(uid)` | uid | `uid_tlb_record_by_uid[uid]` 上下文记录 | uid 发射后记录 vpn、s2xlate、runtime CSR snapshot | `get_main_transaction()`、`get_mmu_csr_snapshot()` |
| `update_uid_tlb_records_by_entry(key, entry)` | key、entry | 回填匹配 uid record | L2TLB response 后把 PTE 信息同步给所有匹配的 pending uid record | `memblock_uid_tlb_record::copy_entry_fields()` |
| `tlb_entry_ready_for_uid(uid)` | uid | bit | 判断该 uid 的 TLB record 已登记且 PTE 已回填 | `check_uid()` |
| `apply_raw_csr_runtime(raw, raw_csr_seq)` | raw CSR 和 seq | 更新 runtime CSR | 用 `last_applied_raw_csr_seq` 过滤重复 snapshot，保证 TLB 查表使用最新真实运行时 CSR | `mmu_csr_state.update_from_raw_csr()` |
| `clear_issue_queues()` | 无 | 清 LOAD/STA/STD 三个队列 | reset/test end/recovery 清理队列基础函数 | 无 |
| `issue_queue_contains(target, uid, replay_seq)` | target、uid、seq | bit | 防止同 target、同 uid、同 replay_seq 重复入队 | 无 |
| `push_issue_queue_item(item)` | item | 入 LOAD/STA/STD 队列 | queue item push，已有同 target/uid/replay_seq 时跳过 | `issue_queue_contains()` |
| `delete_issue_queue_entry(target, uid, replay_seq, match_replay_seq)` | target、uid、seq | 删除队列项 | fire/replay/retire 清队列 | 无 |
| `remove_uid_from_issue_queues(uid)` | uid | 删除三个队列中该 uid 所有项，queued bit 清 0 | retire/replay 的兜底清理 | `delete_issue_queue_entry()` |
| `push_ptw_wait_replay(uid, target, issue_epoch, replay_seq, start_cycle)` | replay 信息 | 入 wait replay 队列 | PTW-back replay 先等 TLB response 或 timeout，避免马上重发导致重复非法激励 | `check_uid()` |
| `pop_ready_ptw_wait_replay(timeout, wait_item, timed_out)` | timeout | ready wait item | 扫描 wait replay 队列；TLB response done 或超时就弹出给 exception handler 转 replay_pending | `tlb_entry_ready_for_uid()`、`get_dispatch_service_cycle()` |
| `release_ptw_wait_replay(uid)` | uid | 删除该 uid 的 wait replay | 某 uid 被外部路径完成或清理时释放等待项的 helper | 无 |
| `clear_ptw_wait_replay_by_redirect(redirect)` | redirect payload | 删除被 redirect flush 命中的 wait replay | redirect recovery 时，年轻 uop 的 replay 等待项必须清掉，避免 flush 后又重发 | `rob_need_flush()` |
| `clear_ptw_wait_replay_queue()` | 无 | 清 wait replay 队列 | reset/end 全局清理 | 无 |
| `request_flushsb()`、`arm_scheduled_flushsb()`、`request_scheduled_flushsb_if_due()` | cycle/无 | flushSb pending 状态 | commit sequence 侧 flushSb 调度 | 无 |
| `scheduled_flushsb_pending(cycle_idx)` | cycle | bit | 判断 schedule flushSb 是否还没到发射周期或未发出 | 无 |
| `should_drive_flushsb()` / `mark_flushsb_driven()` / `update_sb_is_empty()` / `flushsb_timed_out()` | cycle/sbIsEmpty/timeout | flushSb waiting 状态 | 驱动 flushSb 后等待 DUT ctrl monitor 回 sbIsEmpty | `issue_blocked_by_global_flush()`、`get_dispatch_service_cycle()` |
| `get_issue_queue_size(target)` | target | 队列长度 | final check/debug 或后续调度扩展可用的统一队列长度读取入口 | 无 |
| `check_main_table_complete()` | 无 | `main_table_ready=1` | 主表完整性检查，并 arm scheduled flushSb | `arm_scheduled_flushsb()` |
| `end_test_check()` | 无 | fatal 未完成状态，关闭 capture | 测试收尾一致性检查 | `raw_monitor_queue_size()`、`clear_raw_monitor_queues()` |

## 12. ROB/LQ/SQ 顺序和 key helper

| 函数 | 输入 | 输出/副作用 | 功能 |
|---|---|---|---|
| `rob_order_util::check_rob_key(key, caller)` | ROB key、caller | 非法 fatal | 检查 `key.value < MEMBLOCK_ROB_SIZE` |
| `rob_to_map_key(key)` | ROB `{flag,value}` | packed map key | active ROB map 的关联数组索引 |
| `lq_to_map_key(key)` / `sq_to_map_key(key)` | LQ/SQ key | packed map key | active LQ/SQ map 的关联数组索引 |
| `rob_advance(base, step)` | ROB key、步数 | 新 ROB key | 按 `MEMBLOCK_ROB_SIZE` 回绕并翻转 flag |
| `rob_is_after(left, right)` | 两个 ROB key | bit | 按 ROB 环形语义判断 left 是否在 right 之后 |
| `rob_need_flush(uop_rob, redirect)` | uop ROB、redirect payload | bit | redirect.flush_itself 命中同 ROB 或 uop 在 redirect 之后则需要 flush |

使用规则：

- 所有 ROB 顺序判断必须用 `rob_order_util`，不能只比较 `value`。
- active map key 是 `{flag,value}`，用于区分回绕前后的同 value。
- `uid` 是主表生命周期索引；ROB/LQ/SQ key 是 DUT 交互索引。二者通过 active map 连接。

## 13. 重要字段表

### 13.1 `common_data_transaction` 公共表/队列

| 字段 | 含义 | 在流程中的作用 |
|---|---|---|
| `main_trans_num` | 主表总行数 | 控制 uid 范围和 end check |
| `next_uid` | 下一个待分配 uid | 主表生成阶段连续分配 |
| `main_table_ready` | 主表可被 agent sequence 消费 | LSQENQ/LINTSISSUE/LSQCOMMIT/L2TLB 等待条件 |
| `main_table_by_uid[]` | uid -> 主表 transaction | 激励静态字段来源 |
| `status_by_uid[]` | uid -> runtime status | 全流程状态机核心 |
| `tlb_entry_by_key[]` | `{vpn,asid,vmid,s2xlate}` -> live TLB/PTE entry | L2TLB responder 按 request 和 runtime CSR 查/建 entry |
| `uid_tlb_record_by_uid[]` | uid -> 发射时 TLB 上下文和 PTE 回填记录 | debug、追溯和 PTW wait replay ready 判断 |
| `mmu_csr_state` | runtime CSR 镜像 | TLB key 的 ASID/VMID/priv 来源 |
| `load_issue_q/sta_issue_q/std_issue_q` | 三个发射队列 | 保存还未成功 fire 的 issue item |
| `exception_event_q` | replay/redirect/fault 复杂事件队列 | exception handler 消费 |
| `pending_redirect_drive_q` | 待驱动 redirect payload | redirect sequence 消费 |
| `ptw_wait_replay_q` | 等 TLB/timeout 的 replay 项 | replay wait PTW 功能 |
| `uid_by_active_rob` | active ROB key -> uid | writeback/redirect 反查 uid |
| `uid_by_lq` | active LQ key -> uid | load writeback/LQ deq 反查 uid |
| `uid_by_sq` | active SQ key -> uid | store feedback/SQ deq 反查 uid |
| `uid_by_tlb_key` | TLB lookup key -> uid | L2TLB request 查表 |

### 13.2 `status_transaction` 状态位

| 字段 | 含义 | 置位/清零点 |
|---|---|---|
| `active` | uid 正在 DUT/TB 生命周期中 | `activate_uid()` 置 1，`retire_active_uid()` 置 0 |
| `enq` | 已完成 admission | `lsq_ctrl_model::commit_allocate*()` 置 1 |
| `tlb_mapped` | uid 已通过 TLB/issue 路由前置检查 | `build_tlb_table_for_active_uid()` 置 1；真实 TLB entry 由 L2TLB request 触发查/建 |
| `queued_load/sta/std` | target 已在发射队列 | `route_target()` 置 1，`mark_issue_fire()`/清理置 0 |
| `load/sta/std_dispatched` | target 已发射到 DUT | `mark_issue_fire()` 置 1，replay 清对应 target |
| `load/sta/std_writeback` | target 有 writeback/feedback | `mark_target_normal_pass()` 或 `mark_target_fault()` |
| `load/sta/std_pass` | target pass | `mark_target_normal_pass()` |
| `load/sta/std_fault` | target fault | `mark_target_fault()` |
| `writeback` | 所有 required target 都完成 writeback | `mark_target_normal_pass()` 聚合置位 |
| `pass` | 所有 required target 正常 pass | `mark_target_normal_pass()` 聚合置位 |
| `fault` | 发生异常 | `mark_target_fault()` |
| `exception_pending` | fault 待处理 | `mark_target_fault()` 置 1，recovery/flush 清 |
| `replay_pending` | backend replay 待重新发射 | `mark_replay_pending()` 置 1，`clear_replay_target_after_fire()` 清 |
| `replay_target_load/sta/std` | replay 需要重发哪些 target | `mark_replay_pending()` 置对应位 |
| `redirect_pending` | redirect recovery 涉及该 uid | `apply_redirect_flush()` 中置位后 retire |
| `flushed` | uid 已被 redirect flush | `apply_redirect_flush()` |
| `rob_commit` | pendingPtr 已推过该 uid | `mark_rob_commit_uid()` |
| `lsq_deq` | LQ/SQ active 映射都释放 | `mark_rob_commit_uid()` 或 release LQ/SQ mapping |
| `success` | uid 完整成功退休 | `try_retire_committed_uid()` |
| `active_lq_mapped/active_sq_mapped` | active LQ/SQ map 是否存在 | `activate_uid()` 置，deq/retire 清 |
| `issue_epoch` / target issue epoch | 发射版本号 | `mark_issue_snapshot()` |
| `replay_seq` | replay 版本号 | `mark_replay_pending()` 中 `bump_replay_seq()` |
| `issue_killed` | 当前发射结果失效 | redirect/clear dispatch result 设置 |

### 13.3 `memblock_issue_q_item_t`

| 字段 | 含义 | 作用 |
|---|---|---|
| `uid` | 主表索引 | 回到主表/status 的唯一索引 |
| `rob_key` | ROB 环形 key | 调度年龄比较和发射 payload |
| `target` | LOAD/STA/STD | 决定进哪个队列和哪个 pipe |
| `send_pri` | 发射优先级 | `send_pri_mode_en=1` 时参与队列内优先级比较；`sample_global_send_pri_en()` 命中时参与跨队列全局最大优先级过滤 |
| `ready_cycle` | 延迟计数 | 非 0 时 item 不 eligible |
| `replay_seq` | replay 版本 | 过滤旧队列项和旧反馈 |
| `has_lqIdx/lq_key` | LQ key 有效性和值 | load 发射和 LQ writeback/deq 反查 |
| `has_sqIdx/sq_key` | SQ key 有效性和值 | store 发射和 SQ feedback/deq 反查 |
| `numLsElem` | LS 元素数 | scalar 当前为 1，atomic/向量扩展时使用 |
| `uop_index/uop_count` | target 内 uop 编号/数量 | 多 uop atomic 和 pipe 映射 |

### 13.4 `memblock_wb_event_t`

| 字段 | 含义 | 作用 |
|---|---|---|
| `valid` | event 有效 | handler 入口过滤 |
| `source` | event 来源 | 区分 LOAD_WB、STA_FEEDBACK、STD_FEEDBACK、MEMORY_VIOLATION、BACKEND_REPLAY 等 |
| `port_id` | DUT monitor port | 同 ROB redirect 选择、debug |
| `target` | LOAD/STA/STD | 更新哪个 target 状态 |
| `uid/has_uid` | 直接 uid | 软件构造 event 可直接指定 |
| `rob_key/has_rob` | ROB key | writeback/redirect 反查 uid |
| `lq_key/has_lq` | LQ key | load writeback 反查 uid |
| `sq_key/has_sq` | SQ key | store feedback 反查 uid |
| `issue_epoch/has_issue_epoch` | 发射版本 | 防旧反馈误更新新发射 |
| `replay_seq/has_replay_seq` | replay 版本 | 防旧 replay/pass 影响新 replay |
| `real_wb_valid` | 正常 pass 标志 | `event_is_normal_pass()` 判断 |
| `has_exception/exception_vec` | 异常信息 | fault 处理 |
| `replay_valid` | backend replay 标志 | 进入 replay pending |
| `redirect_valid/redirect` | redirect 标志和 payload | 进入 redirect recovery |
| `ptw_back_replay` | 由 IQ feedback raw `flushState` 派生的 PTW-back replay 标志 | 可让 replay 等待 TLB response；wb_event 不保存 raw `flushState` 字段 |
| `vector_ls` | 向量 LS 标记 | 当前不支持，handler fatal |
| `cycle` | 采样周期 | last_event_cycle、timeout/debug |

### 13.5 `memblock_sync_pkg` 全局同步字段

| 字段 | 含义 | 在流程中的作用 |
|---|---|---|
| `reset_backend_done` | 后端 reset 完成 | monitor/L2TLB request 采集条件 |
| `dispatch_flush_in_progress` | redirect/flush recovery 中 | driver/sequence 阻塞 admission/issue/commit |
| `dispatch_monitor_capture_en` | monitor raw queue 捕获开关 | 主表 reset 后打开，end check 关闭 |
| `dispatch_flush_epoch` | flush 版本号 | driver 等 ready 期间检测是否跨过 flush |
| `dispatch_service_cycle` | TB 软件服务周期 | replay wait、redirect drive done、flushSb timeout |
| `dispatch_flushsb_waiting_empty` | flushSb 已驱动，等待 sbIsEmpty | ctrl monitor 即使只有 sbIsEmpty 也 push raw_ctrl |
| `l2tlb_responder_active` | L2TLB connect takeover 生效 | L2TLB responder sequence 启动前检查 |
| `raw_int_wb_q/raw_iq_feedback_q/raw_ctrl_q`、`latest_raw_csr` | raw monitor queues 和 CSR latest snapshot | adapter 统一消费/同步并更新状态 |

## 14. 从代码实现角度的主循环伪代码

```systemverilog
// testcase/env 阶段
tc_dispatch_real_smoke.build_phase();
tc_base.build_phase();        // 挂默认 dispatch sequence
memblock_env.build_phase();   // 建 agent/env
memblock_env.connect_phase(); // 接 monitor FIFO

// real smoke 主控 sequence
memblock_main_dispatch_auto_build_main_table_base_sequence.pre_body();
memblock_main_dispatch_auto_build_main_table_base_sequence.body();

// body 内部
seq_csr_common::init();
data = common_data_transaction::get();
lsq_ctrl.reset();
build_main_table();

fork
  // agent default sequence 由 UVM 自动启动
  memblock_lsqenq_dispatch_base_sequence.body();
  memblock_issue_dispatch_base_sequence.body();
  memblock_lsqcommit_dispatch_base_sequence.body();
  memblock_redirect_dispatch_base_sequence.body();
  memblock_l2tlb_base_sequence.body();

  // 主控服务循环
  forever begin
    @(negedge service_vif.clk);
    if (all_transactions_terminal_done()) break;
    memblock_sync_pkg::tick_dispatch_service_cycle();
    monitor_adapter.drain_csr_events();
    monitor_adapter.drain_writeback_events(writeback_handler);
    monitor_adapter.drain_exception_and_redirect_events(writeback_handler);
    exception_handler.process_pending_events();
    issue_sched.route_all_ready_uids();
    if (all_transactions_terminal_done()) break;
  end
join_any

data.end_test_check();
```

## 15. 已复核后的残余风险

以下点是主 agent 按当前源码复核后保留的残余风险，不是文档缺口：

- `memblock_main_dispatch_auto_build_main_table_base_sequence::service_real_dispatch_flow()` 只做 monitor service 和 route，不直接等待 LSQENQ/LINTSISSUE/LSQCOMMIT sequence 的完成；当前完成判据依赖公共状态表和 active map 是否清空，因此 testcase cfg 必须确保 LSQENQ/LINTSISSUE/LSQCOMMIT/REDIRECT/L2TLB sequence 都按预期 enable。
- `memblock_lsqenq_dispatch_base_sequence::admit_non_lsq_if_ready()` 对 `need_alloc=0` 的 atomic/MOU 也调用 `commit_non_lsq_admission()`，该路径会调用 `commit_allocate()` 并 `activate_uid()`，当前行为与 `derive_op_behavior()` 中 atomic 的简化模型一致，但后续如果 atomic 需要真实 LSQ 资源，需要重新审视。
- `exception_event_q` 只由 `exception_redirect_replay_handler::process_pending_events()` 消费；如果未来增加新的复杂 event source，应继续通过 `writeback_status_handler::handle_event()` 或等价分类入口入队，避免新增二次 pop/requeue 路径。
- `memblock_issue_dispatch_base_sequence::assign_issue_items()` 用 target 内候选数组 index 作为 pipe_idx，要求 scheduler 返回的候选数量不超过对应 pipe 数；这个约束目前由 `select_target_candidates(max_count)` 保证。
- `report_deq_mismatch()` 在 `MEMBLOCK_LSQ_RESYNC_ON_MISMATCH=1` 时只降级 warning，不做真正 LSQ 状态重同步。该开关更适合作 debug 继续运行，不适合作功能正确性验收。
- L2TLB lookup 依赖 runtime CSR 的 `update_seq`。CSR monitor capture 如果未开启或 raw CSR 未及时 drain，可能导致 TLB key 使用旧 CSR 镜像。
