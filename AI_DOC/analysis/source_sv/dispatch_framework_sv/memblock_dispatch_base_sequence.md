# memblock_dispatch_base_sequence.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`

## 1. 文件定位

`memblock_dispatch_base_sequence` 是 dispatch 公共测试框架的 base sequence。它本身不直接驱动某一个 DUT interface，而是把主表生成、公共数据初始化、公共 helper 初始化、monitor service、writeback/replay/redirect 事件处理、commit helper 等能力包装成统一 API，给真实 DUT orchestration sequence 和软件 smoke sequence 复用。

可以把它理解成“公共调度工具箱”：

- testcase 或派生 sequence 先调用它初始化公共 helper。
- 它生成或导入主表，把 transaction 放进 `common_data_transaction`。
- LSQ 入队成功后的 `issue_ready` 标记由 `issue_queue_scheduler::prepare_issue_route_for_uid()` 完成。真实 DUT LSQ 入队 sequence 直接调用 scheduler；software smoke 的同名 helper 已下沉到 soft sequence 内部。
- 真正的 TLB entry 不在 base sequence 里提前按 uid 建表，而是在 L2TLB request 到来时由 `common_data_transaction::get_or_create_tlb_entry_by_req()` 按 `{vpn, asid, vmid, s2xlate}` key 查找或创建。
- 真实 issue sequence 直接调用 `issue_queue_scheduler` 选择候选并标记发射，直接调用 `issue_field_assigner` 填 issue xaction；base sequence 不再保存 soft-only issue fire wrapper。
- monitor 采到 DUT 返回事件后，它把 raw event 转成公共事件，再交给 writeback/replay/redirect handler 更新状态。

真实 DUT 接口驱动仍由专门的 agent sequence 完成，例如 `memblock_lsqenq_dispatch_sequence`、`memblock_lintsissue_dispatch_sequence`、`memblock_lsqcommit_dispatch_sequence`、`memblock_l2tlb_base_sequence`、`memblock_redirect_dispatch_sequence`。base sequence 只提供这些 sequence 共同依赖的数据和 helper 调度入口。

## 2. 成员字段

| 字段 | 类型 | 含义 | 设计目的 |
|---|---|---|---|
| `data` | `common_data_transaction` | 公共数据单例句柄。 | 保存主表、状态表、TLB 表、issue queue、feedback event、active ROB/LQ/SQ 映射。 |
| `lsq_ctrl` | `lsq_ctrl_model` | LSQ 资源和 op 行为模型。 | 统一推导 load/store/AMO/prefetch 的 LQ/SQ 占用、路由和 commit 行为。 |
| `issue_sched` | `issue_queue_scheduler` | issue queue 调度 helper。 | 维护 LOAD/STA/STD 三类发射队列，按优先级、ROB 顺序和 replay 状态选择候选。 |
| `field_assigner` | `issue_field_assigner` | issue xaction 字段赋值 helper。 | 把主表字段、依赖字段、后端 meta 字段填入 `lintsissue_agent_agent_xaction`。 |
| `writeback_handler` | `writeback_status_handler` | writeback/pass/fault 事件处理 helper。 | 处理 batch 已放行的 normal pass、fault 和 IQ feedback，更新 status 表。 |
| `monitor_batch_handler` | `dispatch_monitor_batch_handler` | monitor semantic event batch 仲裁 helper。 | 对同一 service cycle 的 writeback/IQ/memoryViolation event 统一 normalize，执行 active redirect 过滤和同批 redirect-first 仲裁。 |
| `exception_handler` | `exception_redirect_replay_handler` | replay/redirect/fault recovery helper。 | 处理 replay 重新入队、redirect freeze/flush/recovery。 |
| `monitor_commit_handler` | `lsq_commit_handler` | monitor 侧 LSQ deq/commit helper。 | 处理 DUT ctrl deq 采样后的 LQ/SQ 释放和 retire 推进。 |
| `monitor_adapter` | `dispatch_monitor_event_adapter` | monitor raw event 适配器。 | 把 raw writeback、IQ feedback、redirect、memory violation、CSR/deq event 转成公共事件。 |
| `manual_main_table_by_rob[int unsigned]` | `main_control_transaction` 关联数组 | 手动主表临时缓存，key 是 rob value。 | 支持 testcase 手动配置主表，导入时按 ROB key 排序分配 uid。 |

## 3. 通俗内部逻辑

这份 base sequence 的内部逻辑可以分成五步。

第一步，`pre_body()` 做公共初始化。它读取 plus 配置快照，拿到 `common_data_transaction` 单例，创建或绑定所有 helper。后续真实接口 sequence 和软件 smoke sequence 都使用同一份 helper 与公共数据表，因此不会出现多个 sequence 各自维护一套状态的问题。

第二步，`build_main_table()` 生成测试输入。如果打开手动主表模式，就从 `manual_main_table_by_rob` 按 ROB 顺序导入；否则按权重随机生成指定数量的 transaction。每条 transaction 都会分配 uid 和 ROB key，并补齐 op class、fuType/fuOpType、地址、send priority、delay 等字段。

第三步，随机主表边生成边维护 recent-window 地址复用队列。每条 transaction 完成基础随机后，`build_random_main_table()` 会先按 uid 距离淘汰 `recent_load_uid_q/recent_store_uid_q` 里的过期候选，再按 `MEMBLOCK_ADDR_REUSE_EN_*` 和四类 `MEMBLOCK_ADDR_REUSE_*` 权重决定是否复用窗口内地址。命中时只复制参考 transaction 的 `src_0/imm` 并重新更新 `vaddr`，随后把当前 transaction 写入主表并按最终 load/store 类型重新入 recent queue。最后 `init_status_for_main_table()` 给每个 uid 建 status 表项。

第四步，真实 DUT flow 中，LSQ 入队 sequence 在 DUT admission 成功后直接调用 `issue_queue_scheduler::prepare_issue_route_for_uid()`。该 helper 会确认 uid 已经 active/enq，把 `MEMBLOCK_STATUS_ISSUE_READY` 置高，并把 uid 放入 LOAD/STA/STD issue queue。`issue_ready` 只表示该 uid 已具备进入 issue 路径的测试框架条件；真实 TLB entry 会等 DTLB 发出 L2TLB request 后，由 L2TLB responder 通过公共表按 key 查找或创建，并在 PTE 回填 uid record 后置 `MEMBLOCK_STATUS_TLB_MAPPED`。software smoke 中的同名准备函数只保留在 `soft_test_memblock_dispatch_smoke_sequence` 内部。

第五步，monitor 和 recovery 路径不断服务 DUT 返回事件。真实 service loop 使用 `collect_monitor_event_batch()` 一次性收集 raw int writeback、IQ feedback 和 ctrl memoryViolation，交给 `dispatch_monitor_batch_handler::process_monitor_event_batch()` 做 normalize、active redirect 过滤和同批 redirect-first 仲裁。只有未被 redirect 覆盖的非 redirect event 才交给 `writeback_status_handler` 落 pass/fault/IQ feedback 状态；redirect/replay/fault 进入 `exception_event_q` 后由 `exception_redirect_replay_task()` 统一处理 replay pending、redirect drive/flush/recovery。

## 4. 生命周期与数据流

| 阶段 | 入口 | 输入 | 输出/副作用 |
|---|---|---|---|
| 初始化 | `pre_body()` | plus 参数、UVM factory、已有公共单例 | 初始化 `seq_csr_common`，绑定 `data` 和各 helper。 |
| 主表准备 | `build_main_table()` | `seq_csr_common::get_use_manual_main_table()`、手动表或随机权重 | 填充 `main_table_by_uid[]`、`status_by_uid[]`，检查主表完整性。 |
| 地址相关性注入 | `apply_addr_reuse_window()` | recent load/store uid queue、uid窗口、地址复用权重 | 在主表生成期修正当前 transaction 类型并可复制窗口内参考地址。 |
| issue route 服务 | `route_all_issue_queues()` | 公共状态表和 issue queue | 扫描 ready uid 并补充路由；真实 admission 后的 `issue_ready` 设置由 scheduler 核心 helper 完成。 |
| monitor event 服务 | `collect_*_events()`、`exception_redirect_replay_task()` | monitor raw queue、feedback event queue | 更新 pass/fault/replay/redirect/flush/retire 状态。 |

## 5. 函数和 task 明细

### 5.1 UVM 生命周期

| 函数/task | 输入 | 输出/返回 | 作用 |
|---|---|---|---|
| `new(name)` | `name`：UVM object 名称，默认 `"memblock_dispatch_base_sequence"`。 | 返回新 sequence 对象。 | 调用 `super.new(name)`，只负责对象构造，不初始化公共数据。 |
| `pre_body()` | 无显式参数；隐式依赖 plus 配置和 UVM factory。 | 无返回；副作用是初始化 `seq_csr_common`、`data`、`lsq_ctrl` 和各 helper。 | 所有派生 sequence 在正式工作前的公共准备入口。它保证后续 API 有可用 helper，并 reset LSQ 软件模型。 |
| `body()` | 无。 | 无。 | 当前只是 skeleton，打印“Task1 skeleton only”。实际 dispatch flow 由派生 sequence 或 agent default sequence 调用本类 API 完成。 |
| `post_body()` | 无。 | 无。 | 调用 `super.post_body()`，保留 UVM 生命周期扩展点。 |

### 5.2 主表生成和导入

| 函数/task | 输入 | 输出/返回 | 作用 |
|---|---|---|---|
| `build_main_table()` | 无显式参数；读取 `seq_csr_common::get_use_manual_main_table()` 和 `get_main_trans_num()`。 | 无返回；副作用是完成主表、状态表和公共表初始化。 | 主表总入口。打开手动模式时调用 `import_manual_main_table()`，否则调用 `build_random_main_table()`。 |
| `build_random_main_table(main_trans_num_i)` | `main_trans_num_i`：随机生成 transaction 数量。 | 无返回；填充 `data.main_table_by_uid[]` 和 `data.status_by_uid[]`。 | reset 公共表，选择 ROB 起始 value 和地址复用 uid 窗口，边生成 transaction 边用 recent load/store queue 尝试地址复用，最后初始化 status 并检查主表完整性。 |
| `import_manual_main_table()` | 无显式参数；输入来自 `manual_main_table_by_rob[]`。 | 无返回；按 ROB key 排序导入主表。 | 手动主表模式入口。它要求至少有一个手动条目，按 rob key 从小到大分配 uid，调用 `post_manual_config()` 和 `validate_main_table_entry()`；地址复用只在随机主表生成期执行。 |
| `clear_manual_main_table()` | 无。 | 无返回；清空 `manual_main_table_by_rob[]`。 | 清除 testcase 之前塞入的手动主表，避免跨 testcase 残留。 |
| `set_manual_main_transaction(rob_key, tr)` | `rob_key`：手动排序 key；`tr`：手动配置的 transaction 句柄。 | 无返回；把 `tr` 存到 `manual_main_table_by_rob[rob_key]`。 | 给 testcase 提供手动主表配置 API。`tr` 不能为空，否则 fatal。 |
| `init_status_for_main_table()` | 无显式参数；依赖 `data.main_trans_num`。 | 无返回；为每个 uid 调 `data.init_status_for_uid(uid)`。 | 主表生成/导入后统一初始化 status 表，保证状态表和主表 uid 范围一致。 |
| `validate_main_table_entry(tr, caller)` | `tr`：待检查 transaction；`caller`：错误上下文字符串。 | 无返回；非法时 fatal。 | 主表入口校验。检查 transaction 派生字段、ROB 范围、vector LS 限制、`numLsElem` 和 op template 合法性。 |

### 5.3 随机 transaction 构造

| 函数/task | 输入 | 输出/返回 | 作用 |
|---|---|---|---|
| `randomize_main_transaction(tr, uid, rob_key)` | `tr`：待随机 transaction；`uid`：分配好的 uid；`rob_key`：分配好的 ROB key。 | 无返回；直接修改 `tr`。 | 完成单条随机主表 transaction 的所有基础字段：uid/ROB、LQ/SQ 初值、异常默认值、op class、fuType/fuOpType、地址、send priority、delay。最后调用校验。 |
| `select_op_class_by_weight()` | 无显式参数；读取 int load/fp load/store/prefetch/AMO 权重。 | `memblock_op_class_e`。 | 按 plus 权重选择操作大类。所有权重为 0 时底层 `rand_weighted5()` fatal。 |
| `apply_minimal_op_template(tr)` | `tr`：已选好 `op_class` 的 transaction。 | 无返回；修改 `tr.fuType/tr.lsq_flow/tr.fuOpType/numLsElem`。 | 根据 op class 套最小合法模板：load/prefetch 走 LDU+LOAD 且 `numLsElem=1`，store 走 STU+STORE 且 `numLsElem=1`，AMO 走 MOU+ATOMIC 且当前 `numLsElem=0`；地址复用改类型时也依赖它清掉旧模板残留。 |
| `apply_legal_addr_template(tr)` | `tr`：待填地址 transaction。 | 无返回；修改 `tr.src_0/tr.imm/vaddr`。 | 在 `paddr_base/paddr_range` 范围内挑一个 64B 对齐地址，写到 `src_0`，`imm=0`，然后更新 vaddr。 |
| `random_load_fuoptype()` | 无。 | 9-bit load `fuOpType`。 | 在 LB/LH/LW/LD/LBU/LHU/LWU 中随机选一个。 |
| `random_store_fuoptype()` | 无。 | 9-bit store `fuOpType`。 | 在 SB/SH/SW/SD 中随机选一个。 |
| `random_prefetch_fuoptype()` | 无。 | 9-bit prefetch `fuOpType`。 | 在 PREFETCH_I/PREFETCH_R/PREFETCH_W 中随机选一个。 |
| `random_amo_fuoptype()` | 无。 | 9-bit AMO/LR/SC `fuOpType`。 | 在 W/D 的 LR、SC、AMOSWAP、AMOADD、AMOXOR、AMOAND、AMOOR、AMOMIN、AMOMAX、AMOMINU、AMOMAXU 中随机选一个。 |
| `randomize_send_pri_value(is_std)` | `is_std`：1 表示生成 STD 优先级，0 表示生成 LOAD/STA 优先级。 | `int unsigned`，范围 0 到 100。 | `send_pri_mode_en=0` 时返回默认值；开启时按 low/mid/high 权重生成 0-33、34-66 或 67-100 区间值。 |
| `randomize_delay_value()` | 无。 | `int unsigned` delay。 | 按 delay 权重生成 0、1-20 或 21-50 的 ready delay。 |
| `rand_percent_hit(percent)` | `percent`：0-100 百分比。 | `bit`：是否命中。 | 百分比随机工具，0 永远不命中，100 及以上永远命中。 |
| `rand_weighted3(w0, w1, w2)` | 三个权重。 | `int unsigned`：返回 0/1/2。 | 三选一权重随机工具，权重总和为 0 时 fatal。 |
| `rand_weighted5(w0, w1, w2, w3, w4)` | 五个权重。 | `int unsigned`：返回 0/1/2/3/4。 | 五选一权重随机工具，权重总和为 0 时 fatal。 |

### 5.4 地址复用和类型判断

| 函数/task | 输入 | 输出/返回 | 作用 |
|---|---|---|---|
| `choose_rob_start_key()` | 无显式参数；读取 ROB start 固定值或随机权重。 | `memblock_rob_key_t`。 | 选择随机主表 uid0 的 ROB 起始 value，初始 flag 固定为 0，后续仍由 `rob_advance()` 连续推进。 |
| `choose_addr_ref_window()` | 无显式参数；读取 fixed/small/medium/large 窗口参数。 | `int unsigned` uid 距离窗口。 | 选择本轮 random main table 的地址复用窗口，最终上限为 `min(MEMBLOCK_LQ_SIZE, MEMBLOCK_SQ_SIZE)`。 |
| `apply_addr_reuse_window(tr, cur_uid, recent_load_uid_q, recent_store_uid_q)` | 当前 transaction、当前 uid、窗口内 load/store uid queue。 | 无返回；可能修正 `tr.op_class/fuType/fuOpType/lsq_flow/src_0/imm/vaddr`。 | 每个 uid 只随机一次 enable，命中后按四类 after 枚举选择一个参考队列，在主表写入前完成类型修正和地址复制。 |
| `set_transaction_ls_kind(tr, make_load)` | transaction 和目标 load/store 类型。 | 无返回；修正 op class 和最小合法模板。 | 第一层类型修正 helper，保证 `fuType/fuOpType/lsq_flow/numLsElem` 与最终 load/store 类型一致。 |
| `fixup_after_addr_reuse(tr, ref_tr, copy_addr, caller)` | 当前 transaction、可选参考 transaction、是否复制地址。 | 无返回；更新 `vaddr` 并校验。 | 第二层复用后 fixup helper，负责复制 `src_0/imm`、重算 `vaddr`，并调用 `validate_main_table_entry()`。 |
| `prune_recent_uid_q(uid_q, cur_uid, addr_ref_window)` | recent uid queue、当前 uid、窗口。 | 无返回；删除过期 uid。 | 从队头淘汰 `cur_uid - ref_uid > addr_ref_window` 的候选，避免用已经离当前太远的 LSQ entry。 |
| `random_pick_recent_uid(uid_q, ref_uid, delete_after_pick)` | recent uid queue、输出 ref uid、是否删除。 | `bit`：是否选到候选。 | 从窗口内候选随机取 uid；同类型复用会删除队列项，跨类型复用保留队列项。 |
| `push_recent_uid(tr, uid, recent_load_uid_q, recent_store_uid_q)` | 最终 transaction 和 uid。 | 无返回；把 uid 推入 load 或 store recent queue。 | 当前项写入主表后按最终类型入队，供后续 uid 使用。 |
| `is_load_main_tr(tr)` | `tr`：主表 transaction。 | `bit`。 | 判断 transaction 是否属于 load 类主表项。当前 INT_LOAD、FP_LOAD、PREFETCH 都算 load 类路径。 |
| `is_store_main_tr(tr)` | `tr`：主表 transaction。 | `bit`。 | 判断 transaction 是否属于普通 STORE op class。 |
| `is_vector_ls_main_tr(tr)` | `tr`：主表 transaction。 | `bit`。 | 判断 fuType 是否为 VLDU/VSTU/VSEGLDU/VSEGSTU。当前 base sequence 校验中不支持 vector LS。 |
| `is_load_fuoptype(fuOpType)` | `fuOpType`：9-bit LSU op。 | `bit`。 | 转发到 `lsq_ctrl_model::is_load_fuoptype()`。 |
| `is_store_fuoptype(fuOpType)` | `fuOpType`：9-bit LSU op。 | `bit`。 | 判断 store 或 CBO fuOpType。用于 store template 校验。 |
| `is_prefetch_fuoptype(fuOpType)` | `fuOpType`：9-bit LSU op。 | `bit`。 | 转发到 `lsq_ctrl_model::is_prefetch_fuoptype()`。 |
| `is_amo_fuoptype(fuOpType)` | `fuOpType`：9-bit LSU op。 | `bit`。 | 转发到 `lsq_ctrl_model::is_amo_fuoptype()`。 |
| `derive_op_behavior(tr)` | `tr`：主表 transaction。 | `memblock_op_behavior_t`。 | 转发到 `lsq_ctrl_model::derive_op_behavior(tr)`，得到 LQ/SQ 占用、路由、commit 类型等行为。 |

### 5.5 issue queue 路由

| 函数/task | 输入 | 输出/返回 | 作用 |
|---|---|---|---|
| `route_all_issue_queues()` | 无。 | 无返回；扫描所有 ready uid。 | 转发到 `issue_sched.route_all_ready_uids()`，用于 real smoke 服务循环里补路由。 |

### 5.6 issue xaction 字段赋值

base sequence 不再保留 `assign_main_issue_fields()`、`assign_issue_dep_fields()`、`assign_backend_meta_fields()`、`assign_issue_item_fields()` 这类纯转发 wrapper。真实 DUT issue flow 由 `memblock_lintsissue_dispatch_sequence::assign_issue_items()` 直接调用 `issue_field_assigner::assign_issue_item_fields()`，字段赋值细节统一维护在 [issue_field_assigner.md](./issue_field_assigner.md)。

### 5.7 monitor、writeback、replay、redirect

| 函数/task | 输入 | 输出/返回 | 作用 |
|---|---|---|---|
| `collect_csr_runtime_events()` | 无显式参数；输入来自 CSR latest snapshot。 | 无返回；更新 runtime CSR snapshot。 | 调用 `monitor_adapter.drain_csr_events()`。该入口只同步 CSR runtime，不消费 sfence/hfence，TLB 建表前会调用，保证 lookup key 使用当前真实 CSR 上下文。 |
| `collect_runtime_context_events()` | 无显式参数；输入来自 CSR latest snapshot 和 raw sfence queue。 | 无返回；先更新 runtime CSR snapshot，再消费 sfence/hfence。 | 统一 service loop 使用该入口显式执行 `drain_csr_events()` -> `drain_sfence_events()`，保证 sfence/hfence entry 级失效使用最新 CSR runtime，同时避免其它 CSR-only 路径隐式消费 sfence。 |
| `collect_monitor_event_batch()` | 无显式参数；输入来自 raw int writeback、raw IQ feedback、raw ctrl queue。 | 无返回；可能更新 pass/fault/IQ feedback 状态或 enqueue redirect/replay/fault recovery event。 | 真实 monitor service 入口。同一轮先收集所有 semantic event，再由 batch handler 选择 oldest redirect、过滤覆盖范围内的 stale event，并处理未覆盖 event。 |
| `exception_redirect_replay_task()` | 无。 | 无返回；更新 replay pending、redirect drive/flush/recovery 状态。 | 调用 `exception_handler.process_pending_events()`。这是 replay 重新入队和 redirect 恢复流程的公共入口。 |

## 6. 关键约束和错误处理

- `data` 为空时，大多数入口会重新取 `common_data_transaction::get()`；但主表相关 API 仍要求调用顺序正确。
- `build_random_main_table()` 和 `import_manual_main_table()` 都会先 `reset_all_tables()`，因此一轮测试内应先建表再驱动真实接口。
- 地址复用只在 `build_random_main_table()` 生成单条 transaction 后、写入主表前执行；手动主表不再有旧后处理入口。
- 手动主表模式下，`manual_main_table_by_rob` 不能为空，且每个 transaction 不能为空。
- `validate_main_table_entry()` 是主表入口的最后防线：ROB 越界、vector LS、op class 和 fuType/fuOpType/lsq_flow 不匹配、`numLsElem` 不符合 op behavior 都会 fatal。
- 随机权重工具 `rand_weighted2()` / `rand_weighted3()` / `rand_weighted4()` / `rand_weighted5()` 不允许全部权重为 0，避免随机结果没有定义。

## 7. 和其它 class 的关系

| 关联 class | base sequence 调用方式 | 关系 |
|---|---|---|
| `common_data_transaction` | `common_data_transaction::get()`、`set_main_transaction()`、`get_status()` 等。 | 所有状态的 owner，base sequence 不另存一套状态。 |
| `lsq_ctrl_model` | `get()`、`reset()`、`derive_op_behavior()`。 | 提供 op 行为和 LSQ 资源语义。 |
| `tlb_map_builder` | base sequence 不直接持有；由 `common_data_transaction::build_tlb_entry_for_key()` 在 L2TLB req miss 时创建并调用。 | 负责把 DTLB request 的 `vpn/s2xlate` 和 CSR runtime snapshot 转成 by-key TLB entry。 |
| `issue_queue_scheduler` | `route_uid()`、`route_all_ready_uids()`、`select_issue_candidates()`、`mark_issue_fire()`。 | 负责 issue queue 的拆分、选择和发射状态更新。 |
| `issue_field_assigner` | `assign_*_fields()`。 | 负责 lintsissue xaction 字段赋值。 |
| `dispatch_monitor_event_adapter` | `collect_*_events_batch()`、`drain_csr_events()`、`drain_sfence_events()`。 | 把 monitor raw fact 转成公共 event 或 commit/deq 更新；不直接调用 writeback handler。 |
| `dispatch_monitor_batch_handler` | `process_monitor_event_batch()`。 | 统一 normalize monitor semantic event，执行 redirect-first 仲裁和 active redirect stale 过滤。 |
| `writeback_status_handler` | `handle_real_writeback_event()`、`handle_issue_feedback_event()`、`handle_event()`。 | 负责 batch 放行后的 pass/fault/IQ feedback 状态更新；不再处理 monitor redirect 仲裁。 |
| `exception_redirect_replay_handler` | `process_pending_events()`。 | 负责 replay pending、redirect drive/flush/recovery。 |
| `lsq_commit_handler` | 绑定到 monitor adapter。 | 处理 LSQ ctrl deq 和 commit/retire 相关状态。 |

## 8. 简短总结

`memblock_dispatch_base_sequence` 不负责“把某个信号拉高”，它负责“让所有真实接口 sequence 看到同一份正确的数据”。主表怎么来、地址怎么复用、uid 怎么进入 issue queue、monitor event 怎么进入状态表，都在这里通过统一 wrapper 串起来；TLB entry 的实际创建则下沉到 `common_data_transaction`，由 L2TLB request 触发。这样真实 flow 中各个 agent sequence 可以各管一段接口交互，同时共享 `common_data_transaction`，避免状态分裂。
