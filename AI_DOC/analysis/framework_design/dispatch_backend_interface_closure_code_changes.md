# Dispatch Backend Interface Closure 代码修改说明

本文总结 `dispatch_backend_interface_closure_plan.md` 本轮后端接口闭环增强实际涉及的代码修改。本文按“特性”组织：先说明这个特性解决什么问题，再说明相关字段和函数为什么存在、在特性里怎么配合使用。

本文不重复说明原有 LSQ 入队、基础 issue、基础 writeback/replay、基础 deq 和 pendingPtr commit flow，只覆盖本轮为了把 DUT 后端接口事实闭环到 mem_ut 公共状态机所做的增强。

通俗地说，本轮目标不是在 TB 里重做一个 MemBlock，而是补齐下面这条链路：

```text
DUT 接口事件
  -> monitor 采集 raw fact
  -> adapter 转成统一事件
  -> 公共状态机冻结/等待/清理/重发
  -> 必要时通过 agent 再真实驱动 DUT 输入接口
  -> monitor 再采 DUT 后续响应
```

## 0. 特性分类总览

| 特性 | 解决的问题 | 关键文件 |
|---|---|---|
| Raw Monitor Sync 与事件归一化 | monitor 只采接口事实，公共状态机统一消费，避免 reset 期脏事件和多 monitor 顺序不一致。 | `memblock_sync_pkg.sv`、`dispatch_monitor_event_adapter.sv` |
| Redirect Recovery 闭环 | DUT output `memoryViolation` 等 ctrl 事件不能只改软件状态，必须先冻结旧上下文，再真实回灌 input `io.redirect`，最后按 ROB 范围清状态。 | `common_data_transaction.sv`、`exception_redirect_replay_handler.sv`、`memblock_redirect_dispatch_sequence.sv` |
| Writeback/Event 状态归一化 | raw monitor 事件来源很多，需要统一成 `memblock_wb_event_t` 后再决定 pass、fault、replay 或 redirect。 | `memblock_dispatch_types.sv`、`common_data_transaction.sv`、`writeback_status_handler.sv` |
| Redirect input monitor 去反馈 | `io_redirect_*` 是 TB/sequence 驱动 DUT 的 input 接口，monitor 只做 X/Z 检查和预留采样，不再写 raw queue 触发 recovery。 | `redirect_agent_agent_monitor.sv`、`memblock_sync_pkg.sv`、`dispatch_monitor_event_adapter.sv` |
| Issue/LSQ 邻近拍保护 | redirect 可能发生在 issue/LSQ 等 ready 的中间拍，必须区分已被 DUT 接受和未被接受的项。 | `lintsissue_agent_agent_driver.sv`、`memblock_lintsissue_dispatch_sequence.sv`、`lsqenq_agent_agent_driver.sv` |
| flushSb / commit / sbIsEmpty Directed 闭环 | directed case 需要在指定 commit cycle 发 `flushSb`，并等待 DUT 返回 `sbIsEmpty=1`。 | `common_data_transaction.sv`、`lsq_commit_handler.sv`、`memblock_lsqcommit_dispatch_sequence.sv` |
| PTW-back Replay 等 L2TLB response done | PTW-back replay 不能只看 TLB 表项存在，还要等 L2TLB responder 真实回包。 | `dispatch_monitor_event_adapter.sv`、`exception_redirect_replay_handler.sv`、`memblock_l2tlb_base_sequence.sv` |
| 参数、默认挂接和收尾检查 | 新闭环能力需要 plus 控制、默认 sequence 消费者和 real smoke 收尾条件配合。 | `plus.sv`、`seq_csr_common.sv`、`tc_base.sv`、`memblock_dispatch_real_smoke_sequence.sv` |

## 1. Raw Monitor Sync 与事件归一化

### 1.1 这个特性是什么

Raw Monitor Sync 是 monitor 和公共 dispatch 状态机之间的缓冲层。monitor 不直接改 `common_data_transaction`，只把接口上看到的事实写入 raw queue；adapter 再把 raw fact 转成统一的 `memblock_wb_event_t` 或 LSQ deq 更新。

这样做的意义是让所有接口事件先进入同一个同步口径，再由 real smoke service loop 按固定顺序处理。例如同一轮里可能同时有 int writeback、IQ feedback、memoryViolation、LSQ deq 和 CSR 变化。如果 monitor 各自直接改状态，事件先后顺序会变得分散，debug 时也很难判断哪个接口事实先影响了软件表。

更具体地说，raw monitor 只回答“DUT 接口上刚才发生了什么”，不回答“这条 transaction 应该成功、重发还是 flush”。这个分层很重要：monitor 位于接口旁边，最适合稳定采样 pin；公共状态机位于 sequence/base_seq，才掌握 uid、active map、issue_epoch、replay_seq、redirect phase 等上下文。如果 monitor 直接更新状态表，它既容易在 reset 或 flush 后误改过期 uid，也会把多个接口的先后关系散落在不同 monitor 中。本特性把 monitor 输出先放进 raw queue，再由 adapter 在固定 service cycle 里统一消费，等于给所有接口事实加了一个“同步缓冲区”。

### 1.2 相关源码修改

相关文件：

- `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_event_adapter.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv`

#### 添加/修改的字段

```systemverilog
bit dispatch_monitor_capture_en = 1'b0;
longint unsigned dispatch_service_cycle = 0;
dispatch_raw_int_wb_t      raw_int_wb_q[$];
dispatch_raw_iq_feedback_t raw_iq_feedback_q[$];
dispatch_raw_ctrl_t        raw_ctrl_q[$];
dispatch_raw_csr_t         latest_raw_csr;
bit                        latest_raw_csr_valid;
int unsigned               latest_raw_csr_seq;
```

| 字段 | 含义 | 为什么需要 | 在本特性中的功能 |
|---|---|---|---|
| `dispatch_monitor_capture_en` | raw monitor 入队总开关。 | reset 期、表未初始化或 test 收尾时，接口可能还有变化，但这些不一定是有效 transaction 事件。 | `push_raw_*()` 入口统一检查它，只有采样窗口打开时才允许 raw fact 进入队列。 |
| `dispatch_service_cycle` | real dispatch service loop 的软件周期。 | 多个特性需要统一的“处理轮次”，不能各自用 `$time` 或 delta cycle 判断。 | redirect freeze、flushSb wait、PTW wait 都用它判断跨 cycle 和 timeout。 |
| `raw_int_wb_q` | int writeback raw queue。 | load/store writeback port 是 DUT 输出事实，先缓存再统一解析 uid。 | `drain_writeback_events()` 消费后转成 writeback/fault event。 |
| `raw_iq_feedback_q` | IssueQueue feedback raw queue。 | STA/STD feedback 既可能代表 pass，也可能代表 replay。 | adapter 根据 `hit/flush_state/source_type` 归一成 pass 或 replay event。 |
| `raw_ctrl_q` | ctrl raw queue。 | ctrl 里混合了 LSQ deq、memoryViolation、sbIsEmpty，需要统一分发。 | adapter 先更新 deq/sbIsEmpty，再把 memoryViolation 转成 redirect event。 |
| `latest_raw_csr/latest_raw_csr_seq` | CSR/TLB runtime latest snapshot。 | L2TLB lookup 依赖最新 runtime CSR snapshot。 | `drain_csr_events()` 读取最新 CSR 和 seq，`common_data_transaction` 用 `last_applied_raw_csr_seq` 统一去重后更新 `mmu_csr_state`，再处理依赖 CSR 的 lookup/replay。 |

这些 raw queue 的共同设计原则是“保留事实，不提前解释”。例如 `raw_int_wb_q` 只记录 port、ROB/LQ/SQ key 和 exception vector，不在 monitor 里判断这是不是 load pass；`raw_ctrl_q` 只记录 `memoryViolation`、deq 和 `sbIsEmpty`，不在 monitor 里直接执行 flush。这样做的代价是多了一层 adapter，但收益是所有状态变化都能在一个 service loop 中按顺序复现，后续 debug 时可以从 raw queue 追到 unified event，再追到状态表更新。

`dispatch_raw_iq_feedback_t` 中新增或强化的字段：

| 字段 | 含义 | 为什么需要 | 在本特性中的功能 |
|---|---|---|---|
| `hit` | IQ feedback 是否成功。 | XiangShan feedback 中 `hit=1` 对应 final success，`hit=0` 对应 failed/replay。 | adapter 用它设置 `real_wb_valid` 或 `replay_valid`。 |
| `flush_state` | IQ feedback 携带的 PTW-back 元信息。 | `flushState` 不是普通 redirect，不能单独把 hit feedback 变成 replay。 | adapter 只在 `STA && !hit && flush_state` 时设置 `ptw_back_replay`。 |
| `source_type` / `vector_feedback` | feedback 来源分类。 | vector feedback 不是本轮 scalar dispatch 闭环的目标。 | adapter 丢弃 vector feedback，避免误更新 scalar transaction。 |

`hit/flush_state/source_type/vector_feedback` 这些字段看起来像是 IQ feedback 的细节，但它们决定了 adapter 能否把 STA/STD feedback 解释对。特别是 `flush_state` 容易被误解成 redirect 标志；当前实现明确把它当成 PTW-back replay 的辅助信息，只有 `STA && !hit && flush_state` 才会进入 PTW wait 相关 flow。这个约束能避免“hit=1 但带 flush_state”时被错误重发。

`dispatch_raw_ctrl_t` 中新增或强化的字段：

| 字段 | 含义 | 为什么需要 | 在本特性中的功能 |
|---|---|---|---|
| `memory_violation_*` | DUT `memoryViolation` 的 raw payload。 | memoryViolation 会触发 redirect/recovery，需要保留 ROB、target、level。 | adapter 把它转换成统一 redirect event。 |
| `sb_is_empty` | DUT 当前 SBuffer empty 状态。 | flushSb 发出后完成条件来自 DUT，不应由 TB 自己假设。 | `update_sb_is_empty()` 用它结束 flushSb waiting。 |

`raw_ctrl_t` 是本轮闭环里比较特殊的一类 raw fact，因为它同时承载三类含义：LSQ deq 表示 DUT 释放 LQ/SQ 项，`memoryViolation` 表示需要 redirect recovery，`sb_is_empty` 表示 directed flushSb 是否完成。文档中把它放在 Raw Monitor Sync 下，是因为它的采样入口仍然是 raw monitor；真正的业务语义会在后续 flushSb、Redirect 和 LSQ commit 章节分别解释。

#### 添加/修改的函数/task

```systemverilog
function void push_raw_int_wb(input dispatch_raw_int_wb_t item);
function bit  pop_raw_int_wb(output dispatch_raw_int_wb_t item);
function void clear_raw_monitor_queues();
function void tick_dispatch_service_cycle();
function longint unsigned get_dispatch_service_cycle();
```

- `push_raw_*()` / `pop_raw_*()` 的功能：它们是 monitor 和 adapter 的唯一交接点，职责是把“接口事实”可靠地放入/取出对应 raw queue。实现上，`push_raw_*()` 会先检查 `dispatch_monitor_capture_en` 和 `item.valid`，只有 real dispatch flow 打开采样窗口后才允许入队；`pop_raw_*()` 在队列为空时返回空 raw 结构，避免调用方读到未初始化字段。它们不做 pass/replay/redirect 判断，这一点是本特性的核心边界。
- `clear_raw_monitor_queues()` 的功能：它负责在 reset table、test 收尾或重新开始前清空所有 raw queue、清除 CSR latest snapshot，并把 `dispatch_service_cycle` 清零。这个函数的意义不是简单清数组，而是切断“上一轮接口事实”和“当前主表状态”之间的联系；如果不清，flush 后迟到的 writeback 或上一轮 CSR snapshot 可能命中新的 active uid。
- `tick_dispatch_service_cycle()` / `get_dispatch_service_cycle()` 的功能：它们给 redirect freeze、flushSb wait、PTW wait 等逻辑提供同一套软件周期。实现上 real smoke 每次 `service_monitor_once()` 先 tick，然后收集/消费 monitor event；因此所有等待和 timeout 都按同一 service loop 计数，不依赖仿真 delta 或具体 agent clocking block 的先后。

```systemverilog
function dispatch_raw_int_wb_t      make_empty_raw_int_wb();
function dispatch_raw_iq_feedback_t make_empty_raw_iq_feedback();
function dispatch_raw_ctrl_t        make_empty_raw_ctrl();
function dispatch_raw_csr_t         make_empty_raw_csr();
function bit raw_csr_payload_changed(input dispatch_raw_csr_t prev,
                                     input dispatch_raw_csr_t cur);
```

- `make_empty_raw_*()` 的功能：它们在本特性中承担“安全默认值”的职责。raw queue 是跨 monitor 和 adapter 的边界，队列为空时必须给出明确的无效对象，而不是让 output 参数保留旧值。实现逻辑是逐字段设置 `valid=0`、key/value 清零、cycle 清零。这样 adapter 后续只需要先判断 `valid`，不会因为旧 `rob_value` 或旧 `exception_vec` 被误当成新事件。
- `raw_csr_payload_changed()` 的功能：它在 CSR raw monitor 中承担“去重”和“变化识别”的职责。CSR/TLB 配置本质上是 level 状态，如果 monitor 每拍都刷新完整 CSR snapshot，L2TLB lookup 和 replay wait 会被重复状态淹没。实现上它比较 satp/vsatp/hgatp、priv 位、PBMT 位以及 changed pulse，只有值变化或变化脉冲出现时才刷新 latest CSR snapshot 并递增 seq。这样 latest CSR 表示“CSR runtime 当前最新状态”，而不是普通时钟采样队列。

```systemverilog
function bit raw_rob_to_key(...);
function bit raw_lq_to_key(...);
function bit raw_sq_to_key(...);
function bit event_has_active_uid(input memblock_wb_event_t wb_event);
function void apply_raw_ctrl_deq(input dispatch_raw_ctrl_t raw);
function void drain_csr_events();
```

- `raw_rob_to_key()` / `raw_lq_to_key()` / `raw_sq_to_key()` 的功能：它们在 adapter 中承担“接口字段转公共 key”的职责。DUT monitor 看到的是 `valid/flag/value`，公共状态表反查 uid 使用的是 `memblock_*_key_t`。这些函数把两种表示统一起来，并返回 valid，后续 `resolve_uid_for_event()` 才能同时利用 ROB/LQ/SQ 多个索引做一致性检查。
- `event_has_active_uid()` 的功能：它是 adapter 转换 raw fact 前的防误更新闸门。只有 raw event 能通过 active ROB/LQ/SQ map 命中当前 active uid，才允许继续进入 writeback/replay/redirect flow。它解决的问题是：DUT 可能在 flush 后仍吐出迟到 writeback，monitor 也可能在 reset 窗口附近采到旧事件；这些事件如果不检查 active uid，就会错误更新新 transaction 的状态。
- `apply_raw_ctrl_deq()` 的功能：它在 raw ctrl 处理链路中承担“先处理资源释放和 sbIsEmpty，再处理异常”的职责。实现上先调用 `data.update_sb_is_empty(raw.sb_is_empty)`，再根据 `lq_deq/sq_deq` 和对应 ptr 调用 `lsq_commit_handler.apply_raw_ctrl_deq()`。这样即使同一个 ctrl raw event 还带 `memoryViolation`，LSQ deq 和 flushSb empty 这些接口事实也不会丢。
- `drain_csr_events()` 的功能：它负责把 latest CSR snapshot 和 seq 交给 `common_data_transaction`。实现上 adapter 在 drain writeback 和 drain redirect/ctrl 前都会先同步 CSR；公共数据侧用 `last_applied_raw_csr_seq` 保证同一个 snapshot 只应用一次。这样后续 L2TLB lookup、PTW-back replay wait、TLB response done 判断使用最新 CSR 运行态，而不是初始 CSR 或 FIFO 中滞留的旧 snapshot。

```systemverilog
task drain_writeback_events(input writeback_status_handler writeback_handler);
task drain_exception_and_redirect_events(input writeback_status_handler writeback_handler);
```

- `drain_writeback_events()` 的功能：它是 writeback 类 raw fact 进入状态机的入口，处在“monitor 入队”之后、“writeback_status_handler 更新状态”之前。实现逻辑是先读取 latest CSR snapshot 并由公共数据侧按 seq 去重应用，保证 runtime CSR 最新；再循环取 `raw_int_wb_q` 和 `raw_iq_feedback_q`，分别调用转换函数生成 `memblock_wb_event_t`；转换成功后交给 `writeback_handler.handle_event()`。它不直接改状态，而是让 writeback handler 根据 event 类型决定 normal pass、fault、replay 或 redirect 延迟处理。
- `drain_exception_and_redirect_events()` 的功能：它是 `memoryViolation` 和 ctrl sideband 事件进入状态机的入口，处在 writeback drain 之后、exception/replay handler 之前。实现逻辑是消费 raw ctrl，先应用 deq 和 `sbIsEmpty`，再把 `memoryViolation` 转成 redirect event。这个顺序保证普通资源释放不会被异常处理吞掉，同时 redirect 类事件最终都进入同一个 recovery handler。

## 2. Redirect Recovery 闭环

### 2.1 这个特性是什么

Redirect Recovery 闭环负责处理 DUT output `memoryViolation` 等会改变后端执行路径的事件。`io_redirect_*` 是 TB 回灌给 DUT 的 input 接口，不作为 recovery 触发源。它不是简单地把某个 transaction 标成失败，而是要完成四步：

1. 冻结新的 admission/issue/commit，防止旧上下文继续进入 DUT。
2. 把 redirect payload 交给 redirect sequence，真实驱动 DUT 的 `io.redirect`。
3. 确认 redirect 已经 drive 完成，并跨过一轮 service cycle。
4. 按 ROB 年龄范围清理软件 active transaction、issue queue 和等待队列。

举例：DUT 返回一个较老 ROB 的 memoryViolation。如果 TB 只在软件表里把 younger transaction 清掉，却没有真实 drive `io.redirect`，DUT 仍可能继续按旧路径执行；反过来，如果 TB 只 drive redirect 但不清软件表，后续 monitor event 又会命中过期 uid。这个特性就是把两边做成一个闭环。

本特性的核心原则是：软件状态恢复必须落后于真实 redirect drive。也就是说，TB 先承认“需要 redirect”，然后冻结发射和 commit，再通过 redirect agent 把 payload 打到 DUT，等 redirect sequence 确认 drive 完成并跨过一个 service cycle 后，才清理公共表项。这个顺序比“采到 redirect 后马上删表”更保守，但能保证 DUT 和 TB 看到的是同一个 recovery 边界。

### 2.2 相关源码修改

相关文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_event_adapter.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/exception_redirect_replay_handler.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_redirect_dispatch_sequence.sv`

#### 添加/修改的字段

```systemverilog
typedef struct packed {
    bit                valid;
    bit                flush_itself;
    bit                level;
    memblock_rob_key_t rob_key;
} memblock_redirect_payload_t;
```

| 字段 | 含义 | 为什么需要 | 在本特性中的功能 |
|---|---|---|---|
| `valid` | payload 是否是真实 redirect。 | redirect payload 会在 queue、active 状态和 xaction 之间传递，需要区分空值和有效事件。 | `request_redirect_flush()`、`push_redirect_drive()`、`assign_redirect_xaction()` 都依赖它防止误驱动。 |
| `rob_key` | redirect 的 ROB 锚点。 | flush 范围按 ROB 年龄判断，只知道“发生 redirect”不够。 | `rob_need_flush()` 用它决定哪些 active uid 需要被清掉；redirect sequence 用它填 `robIdx` pin。 |
| `level` | RTL 暴露的 redirect level 字段。 | 当前 DUT 顶层接口没有独立 `flushItself` pin，只暴露 `level`。 | redirect driver 最终驱动到 `io_redirect_bits_level`。 |
| `flush_itself` | 软件侧“是否 flush redirect 点自身”的语义。 | TB 清软件状态时需要明确边界，不能只靠 raw level 名字推断。 | `rob_need_flush()` 使用它。当前由 `memoryViolation.bits.level(0)` 派生，和 Scala `RedirectLevel.flushItself(level)` 语义一致。 |

`memblock_redirect_payload_t` 是 redirect recovery 的最小公共载体。它不保存完整 transaction，也不保存 replay/fault 细节，只保存 recovery 边界真正需要的内容：这个 redirect 是否有效、以哪个 ROB 为锚点、flush 是否包含锚点自身、真实接口的 level 是什么。这样 payload 可以同时被 exception handler、redirect sequence 和 software flush 复用，避免每个模块对 redirect 语义各解释一遍。

```systemverilog
typedef enum int unsigned {
    MEMBLOCK_REDIRECT_PHASE_IDLE,
    MEMBLOCK_REDIRECT_PHASE_DETECTED,
    MEMBLOCK_REDIRECT_PHASE_FREEZE_REQUESTED,
    MEMBLOCK_REDIRECT_PHASE_REDIRECT_DRIVEN,
    MEMBLOCK_REDIRECT_PHASE_STATE_FLUSH_APPLIED
} memblock_redirect_phase_e;
```

| 字段/状态 | 含义 | 为什么需要 | 在本特性中的功能 |
|---|---|---|---|
| `redirect_phase` | redirect recovery 的阶段状态。 | redirect 不能在采到事件后立刻清软件表，需要等真实回灌完成。 | `advance_active_redirect()` 根据 phase 判断是否可以 apply flush。 |
| `active_redirect` | 当前唯一正在处理的 redirect payload。 | recovery 期间需要全局知道正在处理哪个 redirect。 | issue/commit 阻塞判断读取它；flush apply 时按它清状态。 |
| `flush_in_progress` | `common_data_transaction` 内部 flush 标志。 | 公共数据层自己要阻止 issue queue、commit candidate 和状态更新继续推进旧上下文。 | `request_redirect_flush()` 置 1，`apply_redirect_flush()` 清 0。 |
| `dispatch_flush_in_progress` | 跨 driver/sequence 的全局 flush 标志。 | issue/LSQ driver 不一定能直接访问 `common_data_transaction`。 | driver 等 ready/canAccept 时看到它就 abort 未完成 xaction。 |
| `dispatch_flush_epoch` | 全局 flush 版本号。 | driver 等 ready 期间可能跨多个 cycle，需要知道上下文是否换代。 | redirect freeze 时递增，xaction 用快照判断是否跨 flush。 |
| `pending_redirect_drive_q` | 等 redirect sequence 消费的 payload 队列。 | recovery helper 不应该直接操作 agent driver。 | handler 写入，`memblock_redirect_dispatch_sequence` 取出后真实 drive。 |
| `redirect_drive_inflight` / `redirect_drive_inflight_payload` | payload 已被取走但还没 ack 的中间状态。 | 只看 pending queue 为空无法说明 drive 已完成。 | 防止重复 pop，并让 `redirect_drive_done_for()` 判断是否仍在路上。 |
| `redirect_drive_done_epoch` / `redirect_drive_done_cycle` | redirect drive 完成次数和完成所在 service cycle。 | apply flush 需要确认 drive 已完成，并跨过一轮处理。 | 避免同一轮里刚 drive 就立刻清状态。 |
| `redirect_freeze_cycle` | 进入 freeze 的 service cycle。 | 如果 redirect sequence 没消费 payload，需要 timeout 暴露问题。 | `advance_active_redirect()` 用它做 freeze timeout。 |

这些状态字段共同描述 redirect recovery 的生命周期。`redirect_phase/active_redirect` 表示“当前正在恢复哪一个 redirect”；`flush_in_progress/dispatch_flush_in_progress/issue_freeze_ack` 是给 admission、issue、LSQENQ、commit 和 driver 的闸门；`pending_redirect_drive_q/redirect_drive_inflight` 描述 payload 是否已经被 sequence 取走；`redirect_drive_done_*` 描述真实 drive 是否已经完成。缺少其中任意一类状态，都会产生边界问题：比如没有 inflight 就可能重复 drive，没有 epoch 就无法让等待 ready 的 issue xaction 发现 flush，没 done cycle 就可能同一轮刚 drive 就立即删表。

#### 添加/修改的函数/task

```systemverilog
function bit convert_raw_memory_violation(...);
```

- `convert_raw_memory_violation()` 的功能：它把 ctrl monitor 采到的 `memoryViolation` 也接入同一条 redirect recovery flow。实现上，它设置 `source=MEMORY_VIOLATION`，构造 `redirect.valid=1` 的 payload，并把 `flush_itself/level` 都从 `memory_violation_level` 派生。这样 memoryViolation 不走一套独立 flush 逻辑，而是和普通 redirect 一样经历 freeze、drive redirect、wait done、apply software flush。

```systemverilog
function void request_redirect_flush(input memblock_redirect_payload_t redirect);
function void push_redirect_drive(input memblock_redirect_payload_t payload);
function bit  try_pop_redirect_drive(output memblock_redirect_payload_t payload);
function void mark_redirect_drive_done(input memblock_redirect_payload_t payload);
function bit  redirect_drive_done_for(input memblock_redirect_payload_t payload);
function bit  redirect_payload_equal(input memblock_redirect_payload_t left,
                                     input memblock_redirect_payload_t right);
function bit  has_pending_redirect_drive();
function void clear_redirect_drive_queue();
function void apply_redirect_flush(input memblock_redirect_payload_t redirect);
function bit  issue_blocked_by_global_flush();
```

- `request_redirect_flush()` 的功能：它是 recovery 的第一步，职责是“冻结旧上下文”，不是立即清表。实现上它检查 payload 有效后，把 `active_redirect` 设置为当前 payload，置起 `flush_in_progress` 和全局 `dispatch_flush_in_progress`，递增 `dispatch_flush_epoch`，设置 `issue_freeze_ack`，并记录 `redirect_freeze_cycle`。这些动作让后续 admission、issue、LSQENQ、commit、flushSb 都能通过统一闸门停下来。
- `push_redirect_drive()` 的功能：它是 recovery 的第二步入口，职责是把“需要 drive redirect”这件事交给 redirect sequence。公共状态机不直接操作 redirect agent pin，而是把 payload 放入 `pending_redirect_drive_q`。这样 handler 只管决策，sequence/driver 只管接口时序，两者通过 queue 解耦。
- `try_pop_redirect_drive()` 的功能：它是 redirect sequence 消费 payload 的入口。实现上只有 pending queue 非空且没有 inflight 时才 pop，pop 后设置 `redirect_drive_inflight` 和 `redirect_drive_inflight_payload`。这个 inflight 状态防止同一个 payload 被多个 sequence 循环重复取走，也让状态机知道 drive 还没有完成。
- `mark_redirect_drive_done()` 的功能：它是 redirect sequence 真实 drive `io.redirect` 后给公共状态机的 ack。实现上会检查 done payload 和 inflight payload 是否一致，清 inflight，递增 `redirect_drive_done_epoch`，记录 `redirect_drive_done_cycle`，如果 payload 等于 `active_redirect`，则把 phase 推到 `REDIRECT_DRIVEN`。这个函数是“真实接口已经打出去”的软件确认点。
- `redirect_drive_done_for()` 的功能：它决定 active redirect 是否可以进入软件 flush。判断不是只看 done epoch，而是同时确认该 payload 不在 pending queue、不在 inflight、phase 已到 `REDIRECT_DRIVEN`，并且当前 service cycle 大于 drive done cycle。最后这个跨 cycle 条件很关键，能避免同一轮 service 中刚 drive redirect 就立刻清软件表，给 monitor/driver 一个稳定边界。
- `redirect_payload_equal()` 的功能：它承担 redirect payload 身份比较。比较内容包含 `valid/flush_itself/level/rob_key`，而不是只比较 ROB value。这样如果同一个 ROB 上出现不同 level 或不同 flush_itself 语义的事件，不会被误认为同一个 done ack。
- `has_pending_redirect_drive()` 的功能：它给 issue/commit/收尾检查提供统一的“redirect drive 通道是否还有事”判断。只要 pending queue 里还有 payload，或已有 payload inflight，旧上下文就不能继续 issue/commit，test 也不能提前结束。
- `clear_redirect_drive_queue()` 的功能：它在 software flush apply 完成后清掉 redirect drive 通道残留。这个函数确保 recovery 回到 idle 时 pending/inflight 不会保留旧 payload，否则下一次 redirect sequence 可能又 pop 到上一次 recovery 的 payload。
- `apply_redirect_flush()` 的功能：它是 recovery 的第四步，职责是按照 redirect ROB 范围清理软件状态。实现上遍历 active status，取每个 uid 的 ROB key，调用 `rob_need_flush()` 判断是否落在 flush 范围内；命中后设置 `redirect_pending/flushed`，清 dispatch result，释放 active ROB/LQ/SQ map，并调用 `clear_ptw_wait_replay_by_redirect()` 清掉等待 L2TLB response 的 replay 项。最后清 redirect drive queue、清 flush 标志、清 active_redirect，让系统回到 idle。
- `issue_blocked_by_global_flush()` 的功能：它是所有发射/commit 任务共用的 recovery 闸门。实现上综合检查 `flush_in_progress`、`active_redirect.valid`、`issue_freeze_ack`、`has_pending_redirect_drive()` 和全局 `dispatch_flush_in_progress`。这样 issue、LSQENQ、commit、flushSb 不需要各自猜 recovery 是否结束，避免某条路径漏挡旧 transaction。

```systemverilog
function bit event_is_redirect(input memblock_wb_event_t wb_event);
function bit event_is_replay(input memblock_wb_event_t wb_event);
function bit event_is_fault(input memblock_wb_event_t wb_event);
function memblock_redirect_payload_t redirect_from_event(input memblock_wb_event_t wb_event);
function bit redirect_event_is_older(input memblock_wb_event_t candidate,
                                     input memblock_wb_event_t best);
task process_pending_events();
function void advance_active_redirect();
function bit select_oldest_redirect(...);
function void requeue_non_redirect_events(input memblock_wb_event_t events[$]);
```

- `event_is_redirect()` / `event_is_replay()` / `event_is_fault()` 的功能：它们在 handler 内承担事件分流职责。adapter 和 writeback handler 都会把不同接口来源统一成 `memblock_wb_event_t`，到这里不再关心来源，只关心语义：redirect 进入 recovery，replay 进入 replay pending 或 PTW wait，fault 进入 fault pending。redirect 分类以 canonical `redirect.valid` 为准；`redirect_valid` 作为兼容/显式标志必须与 `redirect.valid` 一致，不一致时 fatal，`source` 不能单独触发 redirect。
- `redirect_from_event()` 的功能：它把统一 event 中的 redirect payload 抽成 `memblock_redirect_payload_t`。实现上只接受 `redirect.valid=1` 的完整 payload；如果分类后进入该函数但 payload 无效，应 fatal，而不是只靠 `rob_key` 合成 redirect。这样后续 recovery 函数只处理 canonical redirect payload，不需要知道 event 是 `memoryViolation` 还是其他来源。
- `redirect_event_is_older()` 的功能：它用于多个 redirect 同时出现时选最老者。实现上先转 payload，再用 `rob_order_util::rob_is_after()` 考虑 ROB flag/value 回绕关系，而不是直接比较 value。选择最老 redirect 的原因是最老 redirect 的 flush 范围更大，先处理它可以覆盖 younger redirect，减少遗漏。
- `process_pending_events()` 的功能：它是 exception/replay/redirect 统一调度入口。每轮先 `service_ptw_wait_replay()`，再 `advance_active_redirect()`；如果 active redirect 还在处理，直接返回。只有没有 active redirect 时，才 pop pending event，选择最老 redirect 并触发 freeze/drive；如果没有 redirect，才处理 replay/fault。这个顺序保证 redirect 优先，避免一个即将被 flush 的 replay/fault 被提前重新入队。
- `select_oldest_redirect()` 的功能：它在一批 pending event 中找出最老 redirect。实现逻辑是遍历 event queue，过滤非 redirect，再用 `redirect_event_is_older()` 更新 best。它不处理 replay/fault，只负责给 recovery 找到唯一锚点。
- `advance_active_redirect()` 的功能：它推进已经进入 freeze 的 redirect。实现上如果 `redirect_drive_done_for(active_redirect)` 为真，就调用 `apply_redirect_flush()`；否则检查 `MEMBLOCK_REDIRECT_FREEZE_TIMEOUT`，超过后 fatal。这个 timeout 能定位 redirect sequence 没挂接、payload 没被消费、driver 没 ack 等问题。
- `requeue_non_redirect_events()` 的功能：当本轮选中了 redirect 并进入 recovery 时，它把同一批中非 redirect 的 replay/fault 放回 pending queue 头部。这样这些事件不会丢，但也不会在 flush 完成前被处理；如果后续 software flush 清掉了对应 uid，normalize/resolve 阶段会自然丢弃过期事件。
- `MEMBLOCK_REDIRECT_SEQ_EN=0` 时 fatal 的意义：redirect event 需要真实回灌消费者。关闭 redirect sequence 又遇到 redirect 时，如果不早失败，仿真会 freeze 后静默挂住。

```systemverilog
task drive_redirect_payload(input memblock_redirect_payload_t payload);
function void assign_redirect_xaction(input redirect_agent_agent_xaction tr,
                                      input memblock_redirect_payload_t payload);
```

- `assign_redirect_xaction()` 的功能：把统一 payload 映射到 redirect agent xaction。它只设置真实 input pin 对应的 `io_redirect_valid/level/robIdx`；不再携带 self-filter metadata。
- `drive_redirect_payload()` 的功能：真实驱动 `io.redirect`，然后调用 `mark_redirect_drive_done()`。这个 done 是 recovery 状态机进入软件 flush 的确认点。

## 3. Writeback/Event 状态归一化

### 3.1 这个特性是什么

Writeback/Event 状态归一化负责把不同 monitor 或软件任务产生的结果统一成 `memblock_wb_event_t`，再决定它是 normal pass、fault、replay 还是 redirect。它位于 Raw Monitor Sync 和具体状态更新之间，是后端接口闭环的“语义转换层”。

这个特性解决的问题是：DUT 返回结果的接口很多，load writeback、STA/STD IQ feedback、`memoryViolation`、软件 backend replay 都可能改变同一个 uid 的状态。如果每个接口各自直接调用 `mark_target_normal_pass()`、`mark_replay_pending()` 或 `request_redirect_flush()`，就会出现目标分类不一致、replay_seq 过期、flush 后迟到事件误命中等问题。统一 event 的好处是，所有状态更新先经过 active uid 解析、issue_epoch/replay_seq 补齐和 event 分类，再交给对应 handler。`io_redirect_*` 是 TB 驱动 DUT 的 input 接口，不再作为回采 event 来源。

举例：一个 STA 第一次 issue 后收到 `hit=0` replay，随后重新 issue。第二次 issue 的 feedback 如果迟到，必须带着正确的 `issue_epoch/replay_seq` 才能更新状态。`memblock_wb_event_t` 和 normalize 逻辑就是为了让 event 明确属于哪次 issue，而不是只凭 ROB/SQ key 盲目更新。

### 3.2 相关源码修改

相关文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/writeback_status_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_event_adapter.sv`

#### 添加/修改的字段

```systemverilog
typedef struct {
    bit                         valid;
    memblock_wb_event_source_e  source;
    int unsigned                port_id;
    memblock_issue_target_e     target;
    memblock_uid_t              uid;
    bit                         has_uid;
    memblock_rob_key_t          rob_key;
    bit                         has_rob;
    memblock_lq_key_t           lq_key;
    bit                         has_lq;
    memblock_sq_key_t           sq_key;
    bit                         has_sq;
    int unsigned                issue_epoch;
    bit                         has_issue_epoch;
    int unsigned                replay_seq;
    bit                         has_replay_seq;
    bit                         real_wb_valid;
    bit                         has_exception;
    bit [23:0]                  exception_vec;
    bit                         replay_valid;
    bit                         redirect_valid;
    memblock_redirect_payload_t redirect;
    bit                         ptw_back_replay;
    bit                         vector_ls;
    int unsigned                uop_index;
    longint unsigned            cycle;
} memblock_wb_event_t;
```

| 字段 | 含义 | 为什么需要 | 在本特性中的功能 |
|---|---|---|---|
| `valid/source/port_id` | event 是否有效、来自哪个接口源、哪个端口。 | 多个 monitor 都会生成 event，debug 和分类需要知道来源。 | adapter 设置 source，handler 可按 source 打印和区分 load/store/IQ feedback/redirect。 |
| `target` | event 作用到 load、STA 还是 STD。 | 同一条 store 可能有 STA 和 STD 两条路径，不能只按 ROB 更新。 | writeback handler 用它更新对应 target 的 pass/fault/replay 状态。 |
| `uid/has_uid` | 已解析出的主表 uid。 | 有些软件路径已经知道 uid，monitor raw event 则需要反查。 | normalize 后统一填 uid，后续状态更新只按 uid 操作。 |
| `rob_key/has_rob`、`lq_key/has_lq`、`sq_key/has_sq` | event 携带的 DUT ROB/LQ/SQ 身份。 | 不同接口携带的索引不同，需要用它们反查 active uid，并交叉检查一致性。 | `resolve_uid_for_event()` 用这些 key 从 active map 找回 uid。 |
| `issue_epoch/has_issue_epoch` | event 对应哪次 issue 发射。 | replay 后同一 uid/target 可能多次发射，旧 feedback 不能更新新状态。 | normalize 缺省补当前 epoch；replay 后缺 epoch 的 event 会被丢弃。 |
| `replay_seq/has_replay_seq` | event 对应哪一轮 replay 序号。 | 同一 uid replay 多次时，迟到 event 必须被识别为 stale。 | `mark_target_normal_pass()`、`mark_replay_pending()` 等用它过滤过期更新。 |
| `real_wb_valid` | 该 event 表示正常完成/pass。 | raw writeback 和 IQ hit 都可能表示完成，但需要和 replay/fault/redirect 互斥。 | `event_is_normal_pass()` 用它更新 target pass 状态。 |
| `has_exception/exception_vec` | event 携带异常。 | load writeback 可能带 exception vector，不能当成普通 pass。 | writeback handler 调用 `mark_target_fault()` 并把 event 继续送入 exception queue。 |
| `replay_valid` | event 表示需要后端 replay。 | IQ feedback `hit=0` 或后端 replay 不能直接标 success。 | exception/replay handler 把它转成 replay pending 或 PTW wait。 |
| `redirect_valid/redirect` | event 表示 redirect recovery。 | redirect 需要 freeze/drive/flush，不属于普通 writeback pass。 | handler 以 canonical `redirect.valid` 判定 redirect 并优先送入 recovery flow；`redirect_valid` 必须与 payload valid 一致，不一致 fatal。 |
| `ptw_back_replay` | IQ feedback 中由 raw `flushState` 语义化得到的 PTW-back replay 标志。 | PTW-back replay 需要等 L2TLB response done，普通 replay 不需要。 | `event_should_wait_ptw()` 根据它决定是否进入等待队列；wb_event 不再保存 raw `flushState` 字段。 |
| `vector_ls/uop_index/cycle` | vector 标记、uop 序号、采样周期。 | 当前闭环先支持 scalar；cycle 用于 timeout/debug。 | vector event 当前 fatal/drop，cycle 用于日志和等待逻辑。 |

这些字段的设置意义是把“接口事实”转成“可安全更新状态表的事件”。尤其是 `issue_epoch/replay_seq`，它们不是 DUT 原生业务字段，而是 TB 为了防止迟到反馈误更新而附加的软件版本号。如果没有这两个字段，一条 store replay 后，第一轮发射的迟到 STD feedback 可能会把第二轮 replay 状态错误标成 pass。

#### 添加/修改的函数/task

```systemverilog
function memblock_wb_event_t make_empty_wb_event();
function bit feedback_event_is_redirect(input memblock_wb_event_t wb_event);
function bit feedback_event_is_replay(input memblock_wb_event_t wb_event);
function bit feedback_event_has_fault(input memblock_wb_event_t wb_event);
function bit feedback_event_has_action(input memblock_wb_event_t wb_event);
function bit feedback_event_target_is_valid(input memblock_issue_target_e target);
```

- `make_empty_wb_event()` 的功能：它在 event 层提供安全空对象，职责类似 raw 层的 `make_empty_raw_*()`。实现上逐字段清零，并把 source/target 设置为 NONE。这样 `pop_feedback_event()` 队列为空、normalize 失败或 drop stale event 时，都能返回明确的无效 event。
- `feedback_event_is_redirect()` / `feedback_event_is_replay()` / `feedback_event_has_fault()` 的功能：它们负责把 event 语义拆成 redirect、replay、fault 三类。这个分类是后续 handler 分流的基础。redirect 语义只由 canonical `redirect.valid` 判定；`redirect_valid` 是兼容/显式标志，必须与 `redirect.valid` 一致，不一致时 fatal。event 来源可以不同，最终只按语义进入对应状态机，`source` 不能单独触发 redirect。
- `feedback_event_has_action()` 的功能：它判断 event 是否真的需要状态机处理。只有 redirect、replay、fault 或 real_wb_valid 才算有 action。这样空 event 或仅携带 debug 信息的 event 不会进入 pending queue。
- `feedback_event_target_is_valid()` 的功能：它保护非 redirect event 必须落在 load/STA/STD 三类 target 上。redirect 没有具体 issue target，可以 target=NONE；普通 pass/replay/fault 如果 target 不合法，说明 adapter 或 generator 有错误，应 fatal 暴露。

```systemverilog
function bit normalize_feedback_event(input memblock_wb_event_t wb_event,
                                      output memblock_wb_event_t normalized_event);
function void push_feedback_event(input memblock_wb_event_t wb_event);
function bit  pop_feedback_event(output memblock_wb_event_t wb_event);
function void clear_feedback_events();
function bit  resolve_uid_for_event(input memblock_wb_event_t wb_event,
                                    output memblock_uid_t uid);
```

- `normalize_feedback_event()` 的功能：它是 event 进入公共 pending queue 或 writeback handler 前的核心校验和补全步骤。职责是确认 event 有效、有动作、能命中 active uid，并补齐 uid、ROB key、issue_epoch、replay_seq。实现上，如果 event 自带 redirect payload 但没有 ROB key，会从 payload 补 ROB；然后调用 `resolve_uid_for_event()` 反查 active uid；非 redirect event 会检查 target 合法性，并在 replay 后要求 event 携带 issue_epoch/replay_seq，否则丢弃。这个规则能防止 replay 后旧反馈误更新新发射。
- `push_feedback_event()` 的功能：它把需要延迟处理的 event 放入 `exception_event_q`。职责不是简单入队，而是先调用 normalize，只有通过 active uid 和版本检查的 event 才能进入队列。redirect/replay/fault 都通过这里被交给 `exception_redirect_replay_handler`。
- `pop_feedback_event()` / `clear_feedback_events()` 的功能：它们管理 pending event queue。`pop` 给 handler 按顺序消费事件，空队列返回 empty event；`clear` 在 reset/end check 时清空残留，避免上一轮异常或 replay 影响下一轮。
- `resolve_uid_for_event()` 的功能：它用 event 携带的 uid、ROB key、LQ key、SQ key 从 active map 反查主表 uid，并做一致性保护。实现上如果 event 已带 uid，会先确认 uid active；如果带 ROB/LQ/SQ，则分别查 active map，多个 key 命中的 uid 必须一致。这个函数是防止“同 ROB 迟到事件命中新 transaction”的关键保护。

```systemverilog
function bit handle_event(input memblock_wb_event_t wb_event);
```

- `writeback_status_handler::handle_event()` 的功能：它是 normal pass/fault/replay/redirect 的统一入口，处在 adapter 之后、exception/replay handler 之前。实现上先 reject vector LS，再 validate target，再 normalize event。redirect 和 replay 不直接更新 pass 状态，而是 push 到 feedback queue；fault 会先调用 `mark_target_fault()`，再 push 到 feedback queue；normal pass 则调用 `mark_target_normal_pass()`。这样 pass 可以立即落状态，replay/redirect/fault 这种需要额外调度的事件则延迟给专门 handler。
- 旧的 `writeback_status_handler::process_event_queue()` 二次整理入口已删除。`exception_event_q` 中的 replay/redirect/fault 只由 `exception_redirect_replay_handler::process_pending_events()` 消费，避免 writeback handler pop 后再 requeue 的职责混用。

## 4. Redirect input monitor 去反馈

### 4.1 这个特性是什么

`io_redirect_*` 是 redirect sequence/driver 送入 DUT 的 input 接口，不是 DUT output recovery 事件源。redirect monitor 可以继续保留 X/Z 检查、日志、coverage 或 RM 预留，但不再调用 `push_raw_redirect()`，也不再向 dispatch raw queue 反馈 recovery 事件。

因此旧的 Self Redirect Filter 已删除/停用：不再维护 `self_redirect_filter_q`，driver 不再登记 `mark_self_redirect_drive()`，redirect xaction 不再携带 `memblock_dispatch_self_redirect` / `memblock_dispatch_flush_itself` 这类只服务自反馈过滤的 metadata。redirect recovery 的触发源改为 DUT output ctrl monitor 采到的 `memoryViolation` 等输出事件。

### 4.2 相关源码修改

相关文件：

- `mem_ut/ver/ut/memblock/agent/redirect_agent_agent/src/redirect_agent_agent_monitor.sv`
- `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_event_adapter.sv`

#### 添加/修改的函数/task

- `redirect_agent_agent_monitor::mon_data()`：保留 input pin 采样和 X/Z 检查，但删除 raw redirect 构造和 `push_raw_redirect()` 调用。
- `memblock_sync_pkg`：删除 `dispatch_raw_redirect_t`、`raw_redirect_q`、`self_redirect_filter_q`、`push_raw_redirect()`、`pop_raw_redirect()` 和 `mark_self_redirect_drive()`，`raw_monitor_queue_size()` 不再统计 redirect raw queue。
- `dispatch_monitor_event_adapter`：删除 `convert_raw_redirect()` 和 raw redirect drain，`drain_exception_and_redirect_events()` 只从 `raw_ctrl_q` 处理 deq、`sbIsEmpty` 和 `memoryViolation` redirect event。

## 5. Issue/LSQ 邻近拍保护

### 5.1 这个特性是什么

Issue/LSQ 邻近拍保护处理的是 redirect/flush 和 valid/ready 握手重叠的问题。真实 DUT 可能不会一拍 ready，issue driver 需要持续 drive valid 等待 ready；在等待过程中，如果 redirect 发生，已经被 ready 接受的 port 必须记账，没被接受的 port 不能从软件队列里消失。

举例：一个 xaction 同时带 7 个 issue port，其中 port 0/1 已经 ready/fire，port 2 还没 ready。下一轮 service 发现 redirect。正确行为是只把 port 0/1 标成已进入 DUT，port 2 保留或随后被 flush；不能把 7 个都当成功，也不能把 port 0/1 已 fire 的事实丢掉。

这个特性关注的是“边界拍”。在 UVM driver 里，sequence 可能一次构造多个 issue port 的 valid，但 DUT 每个 port 的 ready 独立变化；redirect 又可能在 driver 等 ready 的过程中被另一个 monitor/service loop 发现。没有这层保护时，sequence 只能在 `finish_item()` 返回后粗暴认为整个 xaction 成功或失败，这会把“部分 port 已 fire、部分 port 未 fire”的真实接口行为抹掉。

### 5.2 相关源码修改

相关文件：

- `mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_xaction.sv`
- `mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_driver.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_driver.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`

#### 添加/修改的字段

| 字段 | 含义 | 为什么需要 | 在本特性中的功能 |
|---|---|---|---|
| `memblock_dispatch_wait_ready` | issue xaction 是否要求 driver 处理真实 ready。 | 普通 agent 可打一拍，但 real dispatch issue 必须处理 backpressure 或非阻塞采样。 | sequence 置 1 后，driver 根据 `memblock_dispatch_nonblocking_issue` 进入阻塞等待或单拍采样路径。 |
| `memblock_dispatch_nonblocking_issue` | issue xaction 是否启用非阻塞 ready 采样。 | 非阻塞模式下未 ready item 不能卡住 driver，也不能被误删队列。 | 为 1 时 driver 只采样一次 ready，只把真实 valid&&ready 写入 `fired_mask`，随后清掉剩余 valid 并返回。 |
| `memblock_dispatch_ready_timeout` | 等 ready 的最大 cycle 数。 | DUT 长期不 ready 时不能让仿真无限挂住。 | `wait_dispatch_issue_ready()` 超时 fatal。 |
| `memblock_dispatch_flush_epoch` | xaction 发出时的 flush epoch 快照。 | 等 ready/canAccept 期间可能发生 redirect。 | driver 比较快照和全局 `dispatch_flush_epoch`，变化则 abort 未完成端口。 |
| `memblock_dispatch_fired_mask[6:0]` | 7 个 issue port 中哪些已 fire。 | 多 port xaction 可能部分成功、部分未成功。 | driver 逐 port 置位，sequence 只补记 mask 中的项。 |
| `memblock_dispatch_aborted_by_redirect` | driver 是否因 redirect/flush 中止。 | sequence 需要区分正常完成和 flush abort。 | abort path 下只处理已 fire 项，未 fire 项不误删。 |
| LSQENQ 的 `memblock_dispatch_flush_epoch` / `memblock_dispatch_aborted_by_redirect` | LSQ 入队等待 canAccept 时的同类保护。 | LSQENQ 也可能在等待 DUT 接受期间遇到 flush。 | driver abort 未接受的 enq，sequence 不把它误标为入队成功。 |

这些字段是 sequence 和 driver 之间的“握手结果回传”。`wait_ready` 和 `ready_timeout` 告诉 driver 这不是普通打一拍 xaction，而是要处理真实 ready；`nonblocking_issue` 决定本次 xaction 是阻塞等全部 valid port fire，还是只采样一次 ready；`flush_epoch` 是 xaction 开始时的上下文版本；`fired_mask` 是 driver 观察到的逐端口 fire 事实；`aborted_by_redirect` 告诉 sequence 返回不是正常完成，而是 recovery 打断。没有 `fired_mask`，sequence 无法知道哪些 item 已经进入 DUT；没有 `flush_epoch`，driver 等待或采样 ready 时无法发现全局 recovery 已换代；没有 `aborted_by_redirect`，sequence 会按正常路径误删未 fire 的队列项。

#### 添加/修改的函数/task

```systemverilog
task wait_dispatch_issue_ready(lintsissue_agent_agent_xaction tr);
task drive_dispatch_issue_one_cycle(lintsissue_agent_agent_xaction tr);
function void clear_ready_dispatch_issue_ports(lintsissue_agent_agent_xaction tr);
function void clear_dispatch_issue_ports(lintsissue_agent_agent_xaction tr);
```

- `wait_dispatch_issue_ready()` 的功能：它在 driver 侧承担真实 valid/ready 等待循环，是本特性最核心的执行点。实现上每拍进入 clocking block 后，先调用 `clear_ready_dispatch_issue_ports()` 采样哪些 valid port 已经 ready/fire，并把这些 port 记入 `fired_mask`；随后检查全局 `dispatch_flush_in_progress` 和 xaction 保存的 `memblock_dispatch_flush_epoch` 是否仍一致。如果 flush 已发生或 epoch 变化，就调用 `clear_dispatch_issue_ports()` 清掉剩余 valid，再 drive 一次清空后的 xaction，置 `aborted_by_redirect=1` 返回。最后才继续正常 send_pkt 和 timeout 检查。这个顺序保证“已 fire 的先记账，未 fire 的不再进入 DUT”。
- `drive_dispatch_issue_one_cycle()` 的功能：它在非阻塞模式下只等待一个 driver clocking block，采样一次 `valid&&ready` 并写入 `fired_mask`。采样 ready 前后都会检查 `dispatch_flush_in_progress/flush_epoch`，正常返回前会清掉剩余 valid 并 drive idle；未 ready item 因 `fired_mask=0` 不会被 sequence 删除。
- `clear_ready_dispatch_issue_ports()` 的功能：它负责把 DUT 已接受的 port 从 pending valid 集合中移除。实现逻辑是逐个检查 7 个 issue port：如果该 port valid 且 ready 为 1，就打印 fire log，设置 `memblock_dispatch_fired_mask[port]=1`，并清该 port valid。清 valid 的意义是避免下一拍重复 drive 已被接受的 uop；设置 fired_mask 的意义是让 sequence 返回后只对这些 item 调用状态更新。
- `clear_dispatch_issue_ports()` 的功能：它在 redirect/flush 发生后清掉所有剩余 issue valid，职责是阻止旧上下文继续进入 DUT。实现上把 7 个 issue valid 全部置 0，不修改 fired_mask。这样已经在前面 `clear_ready_dispatch_issue_ports()` 中记录的 fire 事实仍然保留，未 fire 的 port 则被停止 drive。

```systemverilog
function void mark_fired_items(input memblock_issue_q_item_t fired_items[$],
                               input bit [6:0] fired_mask);
function bit mark_issue_fire_already_accepted(input memblock_issue_q_item_t item);
```

- `mark_fired_items()` 的功能：它在 sequence 侧承担“根据 driver 回传的 fired_mask 更新公共状态”的职责。实现上 sequence 原本会保存本轮分配到各 port 的 `fired_items`，driver 返回后只遍历 mask 为 1 的 port，并对对应 item 调用 fire 状态更新。阻塞模式正常完成时 mask 可视为全 1；非阻塞模式和 redirect abort 时只使用 driver 实际记录的 mask。这样未 ready 的 item 不会被误删，已 ready 的 item也不会因为 abort 而丢掉。
- `mark_issue_fire_already_accepted()` 的功能：它用于 flush 已经开始但某些 port 已经被 DUT 接受的场景。普通 `mark_issue_fire()` 表示 sequence 主动选发并成功；该函数更像“补记接口事实”：既然 DUT 已经 ready/fire，TB 必须承认这条 uop 已进入 DUT，即使随后 recovery 会清理它。这个区分让状态表既不漏记已发生 fire，也不会把 abort 后的剩余 item 当成正常发射。

`issue_blocked_by_global_flush()` 在这个特性里承担统一闸门作用。LSQENQ、issue 选项、commit candidate 和 flushSb drive 都通过它看同一套 recovery 状态，避免某条路径漏挡旧 transaction。

## 6. flushSb / commit / sbIsEmpty Directed 闭环

### 6.1 这个特性是什么

flushSb Directed 闭环用于 directed testcase 控制 Store Buffer flush。它支持在指定 commit cycle 发一次 `flushSb`，然后等待 DUT `sbIsEmpty=1`，确认 SBuffer 已经清空。

这个特性和普通 ROB commit 不同：commit sequence 负责驱动 `pendingPtr`，同时也是当前唯一能驱动 `flushSb` 的 sequence。因此 flushSb 必须挂在 LSQ commit flow 里，而不是另起一个 sequence 抢同一个 agent。

举例：某个 fence/CBO directed case 希望第 20 个 commit service cycle 发 `flushSb`。如果第 20 cycle 正好遇到 redirect，正确行为是把请求记为 pending，等 redirect 恢复结束后再 drive；不能因为 early return 错过这个 cycle，也不能在 recovery 中强行插入 flushSb。

这个特性最容易混淆的点是：`flushSb` pulse 发出不等于 Store Buffer 已经清空。真实完成条件来自 DUT 输出的 `sbIsEmpty`。因此本闭环要维护三个阶段：未来某个 cycle 要发、已经到点但还没发、已经发出且正在等 empty。`dispatch_flushsb_waiting_empty` 也属于这个闭环，它只是给 ctrl monitor 的采样提示，不是 Raw Monitor Sync 的通用开关，也不是 redirect recovery 输入回采逻辑的一部分。

### 6.2 相关源码修改

相关文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/lsq_commit_handler.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_monitor.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv`

#### 添加/修改的字段

| 字段 | 含义 | 为什么需要 | 在本特性中的功能 |
|---|---|---|---|
| `flushsb_scheduled_pending` | 已登记未来某 cycle 要发 flushSb。 | request cycle 未到时不能提前发，也不能让 testcase 提前结束。 | real smoke 和 commit sequence activity 判断会等待它。 |
| `flushsb_scheduled_issued` | scheduled request 是否已经转成真实请求。 | 防止同一个 request cycle 重复触发。 | due 后置 1。 |
| `flushsb_scheduled_cycle` | scheduled flushSb 的目标 cycle。 | 需要知道何时从 scheduled 转 pending。 | `request_scheduled_flushsb_if_due()` 使用。 |
| `flushsb_pending` | 请求已到点，但还没真实 drive。 | due cycle 可能撞上 global flush，不能丢请求。 | `should_drive_flushsb()` 允许时才驱动。 |
| `flushsb_waiting_empty` | 已发出 `flushSb`，正在等 `sbIsEmpty=1`。 | 发出 pulse 不代表 SBuffer 已清空。 | 收尾和 timeout 都等待该位清零。 |
| `dispatch_flushsb_waiting_empty` | 给 ctrl monitor 的采样提示。 | `sbIsEmpty` 是 level 信号，可能不伴随 deq/memoryViolation 变化。 | waiting 期间强制 ctrl monitor 继续采 raw ctrl。 |
| `flushsb_start_cycle` | drive flushSb 的 service cycle。 | 等 empty 需要 timeout。 | `flushsb_timed_out()` 计算等待年龄。 |
| `last_sb_is_empty` | 最近一次采到的 `sbIsEmpty`。 | debug 和状态追踪需要知道最后观测值。 | `update_sb_is_empty()` 每次 raw ctrl 更新它。 |

这些字段合起来表达 flushSb 的状态机。`flushsb_scheduled_*` 描述“未来要发”的阶段；`flushsb_pending` 描述“已经到点，等待允许 drive”的阶段；`flushsb_waiting_empty/flushsb_start_cycle/last_sb_is_empty` 描述“已经发出，等待 DUT 完成”的阶段；`dispatch_flushsb_waiting_empty` 则把等待状态广播给 ctrl monitor。没有 scheduled 字段，request cycle 撞上 redirect 时会丢请求；没有 pending 字段，due 后被 global flush 阻塞的请求无法恢复；没有 waiting empty 字段，testcase 可能在 pulse 发出后但 SBuffer 未空时提前结束。

#### 添加/修改的函数/task

```systemverilog
function void request_flushsb();
function void arm_scheduled_flushsb(input int unsigned cycle);
function bit  request_scheduled_flushsb_if_due(input int unsigned cycle_idx);
function bit  scheduled_flushsb_pending(input int unsigned cycle_idx);
function bit  should_drive_flushsb();
function void mark_flushsb_driven(input longint unsigned cycle);
function void update_sb_is_empty(input bit sb_is_empty);
function bit  flushsb_timed_out(input int unsigned timeout);
```

- `request_flushsb()` 的功能：它是“立即需要发 flushSb”的请求入口，职责是把请求转成 `flushsb_pending`。实现上只有 `MEMBLOCK_FLUSHSB_SEQ_EN=1` 时才置 pending；如果 sequence 关闭，则 warning 并忽略。这个保护避免出现 pending 已置位但没有任何 LSQ commit sequence 消费它的死等状态。
- `arm_scheduled_flushsb()` 的功能：它根据 plus 配置登记一个未来 cycle 的 flushSb 请求。实现上如果 sequence 关闭或 cycle 为 0，就清空 scheduled 状态；否则设置 `flushsb_scheduled_pending=1`、`flushsb_scheduled_issued=0` 和目标 cycle。它承担的是 directed testcase 的“定时器”职责。
- `request_scheduled_flushsb_if_due()` 的功能：它在每个 commit cycle 检查 scheduled 请求是否到点，并把 scheduled 转成 pending。实现上如果没有 scheduled、已经 issued、或者当前 cycle 小于目标 cycle，就返回 0；到点后调用 `request_flushsb()`，置 issued 并清 scheduled pending。这个函数必须在 global flush early return 之前调用，否则 redirect 恰好挡住 due cycle 时，请求会永远错过。
- `scheduled_flushsb_pending()` 的功能：它告诉 service/commit loop 未来还有一个尚未触发的 flushSb。这个函数主要用于 activity 判断，防止当前没有 transaction 活动时 sequence 提前退出，导致未来 scheduled flushSb 没机会触发。
- `should_drive_flushsb()` 的功能：它判断当前是否允许真实 drive flushSb。条件是 `flushsb_pending=1`、当前不在 waiting empty、并且 `issue_blocked_by_global_flush()` 为 0。这个函数把 pending 请求和 redirect recovery 闸门组合起来，避免在 recovery 冻结期间强行插入 flushSb。
- `mark_flushsb_driven()` 的功能：它在 LSQ commit sequence 真实 drive `io_ooo_to_mem_flushSb=1` 后更新状态。实现上清 `flushsb_pending`，置 `flushsb_waiting_empty`，记录 `flushsb_start_cycle`，清 `last_sb_is_empty`，并置全局 `dispatch_flushsb_waiting_empty`。这一步标志着 flow 从“等待 drive”进入“等待 DUT empty”。
- `update_sb_is_empty()` 的功能：它由 ctrl raw event 调用，职责是把 DUT 当前 `sbIsEmpty` 回写到公共状态。实现上总是更新 `last_sb_is_empty`；只有 `flushsb_waiting_empty=1` 且 `sb_is_empty=1` 时，才清 `flushsb_waiting_empty` 和 `dispatch_flushsb_waiting_empty`。这保证普通 ctrl 采样不会误结束未开始的 flushSb flow。
- `flushsb_timed_out()` 的功能：它是 waiting empty 阶段的超时保护。实现上如果不在 waiting 或 timeout 为 0，则不超时；否则用当前 service cycle 减 `flushsb_start_cycle` 得到等待年龄，超过 timeout 返回 1。LSQ commit sequence 看到超时后 fatal，避免 directed testcase 永久卡住。

```systemverilog
task send_lsqcommit_cycle(input int unsigned cycle_idx,
                          output bit has_commit);
task drive_flushsb_if_needed(input int unsigned cycle_idx,
                             output bit did_drive);
function bit flushsb_request_pending(input int unsigned cycle_idx);
```

- `send_lsqcommit_cycle()` 的功能：它是 LSQ commit sequence 每拍的主入口，既要处理 ROB commit，也要处理 directed flushSb。实现上先调用 `request_scheduled_flushsb_if_due()`，再检查 global flush；如果被 flush 阻塞，只发 idle xaction，不发 commit 或 flushSb，但 due 请求已经被保存为 pending。这样不会因为 redirect recovery 的 early return 丢掉 scheduled 请求。
- `drive_flushsb_if_needed()` 的功能：它在 commit sequence 中承担真实 drive flushSb 的职责。实现上先检查 `flushsb_timed_out()`，再调用 `data.should_drive_flushsb()`；允许 drive 时创建一个 LSQ commit xaction，只置 `io_ooo_to_mem_flushSb=1`，完成后调用 `mark_flushsb_driven()`。它把“请求状态机”和“真实 agent drive”连接起来。
- `flushsb_request_pending()` 的功能：它把 scheduled、pending、waiting 三个阶段统一视为 activity。这个函数主要影响 commit sequence 的 idle 判断：如果只看 pending，不看 scheduled/waiting，sequence 可能在 future request 未到或 waiting empty 未结束时提前退出。

```systemverilog
if (io_mem_to_ooo_lqDeq != '0 ||
    io_mem_to_ooo_sqDeq != '0 ||
    io_mem_to_ooo_memoryViolation_valid ||
    memblock_sync_pkg::dispatch_flushsb_waiting_empty) begin
    ...
    raw_ctrl.sb_is_empty = io_mem_to_ooo_sbIsEmpty;
end
```

ctrl monitor 修改的功能：等待 flushSb 完成期间，即使没有 deq 或 memoryViolation，也继续推 raw ctrl，让 `sbIsEmpty` 能回到公共状态机。`dispatch_flushsb_waiting_empty` 是 flushSb 闭环状态，不属于 redirect recovery 输入回采逻辑。

real smoke 收尾修改的功能：`all_transactions_terminal_done()` 和 `end_test_check()` 会等待 flushSb pending/waiting 状态清空，防止 testcase 在 flushSb 请求未触发或已 drive 未 empty 时提前结束。

## 7. PTW-back Replay 等 L2TLB response done

### 7.1 这个特性是什么

PTW-back Replay 等待用于区分“TB 已有 TLB 表项”和“L2TLB responder 已经真实给 DUT 回过 response”。在 directed 模式下，STA feedback 如果是 PTW-back replay，不能在 hit=0 后马上重新入队，否则可能早于 L2TLB response，和真实接口时序不一致。

举例：STA 发出后 IQ feedback 返回 `hit=0, flush_state=1`，这代表 PTW-back 相关 replay。TB 的 TLB 表里可能已经有答案，但如果 DTLB 还没有收到 L2TLB responder response，就立即 replay，会绕过真实 request/response 闭环。这个特性让 replay 先进入等待队列，等 response done 或 timeout 后再释放。

这里要特别区分两个概念：TLB 表项存在，表示 TB 有能力构造一个 response；L2TLB response done，表示这个 response 已经通过 L2TLB agent 真正打回 DTLB/DUT。PTW-back replay 等待的是后者。否则测试框架会变成“查到表项就允许 replay”，绕开了 DTLB->L2TLB request、L2TLB_agent->DTLB response 这条真实接口路径。

### 7.2 相关源码修改

相关文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_event_adapter.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/exception_redirect_replay_handler.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_l2tlb_base_sequence.sv`
- `mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv`

#### 添加/修改的字段

| 字段 | 含义 | 为什么需要 | 在本特性中的功能 |
|---|---|---|---|
| `ptw_back_replay` | 当前 replay 是否属于 PTW-back replay。 | directed wait 只影响 PTW-back replay，不影响普通 replay。 | `event_should_wait_ptw()` 只检查该位。 |
| `ptw_wait_replay_q` | 等待 L2TLB response done 的 replay 队列。 | PTW-back replay 不能马上 `mark_replay_pending()`。 | handler 把等待项放入队列，后续 ready/timeout 后释放。 |
| `uid_tlb_record_by_uid` | 每个 uid 的发射上下文和 PTE 回填状态。 | live TLB entry 存在不等于该 uid 已完成回填。 | `tlb_entry_ready_for_uid()` 检查 record `pte_valid`。 |
| `l2tlb_responder_active` | L2TLB responder 接口是否被 mem_ut agent 接管。 | L2TLB agent 在 mem_ut 中代替的是“L2TLB 对 DTLB 的上游 responder”，不是下游 PTW/L2Cache 模型；如果 connect 没接管，sequence 发 response 也不会真正回到 DTLB。 | `L2tlb_agent_connect.sv` 根据 connect-time 宏设置该位；L2TLB sequence 启动时校验它，agent driver 也根据它决定默认 ready 行为。 |

这些字段把 PTW-back replay 拆成“识别、等待、释放”三个阶段。`ptw_back_replay` 是 adapter 根据 raw monitor 的 `flush_state` 派生出的语义化标志，用来识别这个 replay 是否属于 PTW-back；`ptw_wait_replay_q` 保存还不能重新入队的 replay 请求；`uid_tlb_record_by_uid[uid].pte_valid` 是该 uid 已被 L2TLB entry 回填的证据；`l2tlb_responder_active` 则确认当前仿真连接确实由 mem_ut 的 L2TLB agent 接管上游 DTLB 交互。如果 connect 没接管，即使 sequence 创建了 response xaction，也可能没有驱动到 DUT 的 DTLB 接口。

#### 添加/修改的函数/task

```systemverilog
function bit convert_raw_iq_feedback(...);
function bit event_should_wait_ptw(input memblock_wb_event_t wb_event);
```

- `convert_raw_iq_feedback()` 的功能：它在 adapter 中把 STA/STD IQ feedback 转成统一 event，是 PTW-back replay 识别的源头。实现上先丢弃 vector feedback 和未知 target，再根据 `is_sta/is_std` 设置 target/source；`hit=1` 设置 `real_wb_valid`，`hit=0` 设置 `replay_valid`；raw `flush_state` 不作为 wb_event 字段保存，只有 `raw.is_sta && !raw.hit && raw.flush_state` 时才置 `ptw_back_replay`。这避免了把带 flush_state 的成功反馈误判成 replay。
- `event_should_wait_ptw()` 的功能：它在 exception/replay handler 中决定 replay 是否进入 PTW wait 队列。实现上要求 `seq_csr_common` 已初始化、`MEMBLOCK_REPLAY_WAIT_PTW_EN=1`，并且 event 的 `ptw_back_replay=1`。因此普通 backend replay、STD replay 或未启用 directed wait 的 testcase 都不会被这个机制阻塞。

```systemverilog
function void push_ptw_wait_replay(...);
function bit  pop_ready_ptw_wait_replay(...);
function void release_ptw_wait_replay(input memblock_uid_t uid);
function void clear_ptw_wait_replay_by_redirect(input memblock_redirect_payload_t redirect);
function void clear_ptw_wait_replay_queue();
function int unsigned update_uid_tlb_records_by_entry(
    input memblock_tlb_lookup_key_t key,
    input memblock_tlb_entry entry
);
function bit  tlb_entry_ready_for_uid(input memblock_uid_t uid);
```

- `push_ptw_wait_replay()` 的功能：它把不能立即 replay 的 PTW-back event 暂存起来，处在 `handle_replay_event()` 识别 PTW wait 之后。实现上记录 uid、target、issue_epoch、replay_seq 和 start cycle，并检查已有队列中是否已经存在同 uid/target/replay_seq，避免重复入队。保存 issue_epoch/replay_seq 是为了释放后仍能按正确版本调用 `mark_replay_pending()`。
- `pop_ready_ptw_wait_replay()` 的功能：它在每轮 `service_ptw_wait_replay()` 中查找可以释放的等待项。实现上遍历队列，先调用 `tlb_entry_ready_for_uid()` 判断 uid record 是否已经完成 PTE 回填；如果不满足，再计算等待年龄，达到 timeout 也允许释放并返回 `timed_out=1`。timeout 是保守降级机制，防止某些 directed case 没有产生 L2TLB request 时永久卡住。
- `release_ptw_wait_replay()` 的功能：它按 uid 主动删除等待项，承担定点清理职责。当前主要作为公共清理能力保留，后续如果某个 uid 被其他路径明确结束，可以用它避免等待队列继续持有该 uid。
- `clear_ptw_wait_replay_by_redirect()` 的功能：它在 redirect software flush 时清掉被 flush 范围内的 PTW wait 项。实现上遍历队列，若 uid 无效、status 不 active，或 `rob_need_flush(status.get_rob_key(), redirect)` 为真，就删除该等待项。这个函数防止已经被 redirect 清掉的 transaction 后续又从 PTW wait 队列释放成 replay。
- `clear_ptw_wait_replay_queue()` 的功能：它在 reset/end check 这类全局清理场景下清空整个 PTW wait 队列。意义和 raw queue 清理类似：避免上一轮 testcase 的等待 replay 影响下一轮。
- `update_uid_tlb_records_by_entry()` 的功能：它在 L2TLB responder 确定 entry 后，按 key 回填所有匹配且 `pte_valid=0` 的 uid record。
- `tlb_entry_ready_for_uid()` 的功能：它把 uid record 是否已经完成 PTE 回填作为 PTW wait 的统一 ready 判断。

```systemverilog
if (!memblock_sync_pkg::l2tlb_responder_active) begin
    `uvm_fatal(...)
end
...
send_l2tlb_item(resp_tr);
void'(data.update_uid_tlb_records_by_entry(key, entry));
```

L2TLB sequence 修改的功能：启动前确认 connect takeover 已生效；按 request 建/查 by-key entry，发送 response，并按 key 回填 uid record。这里不再从 key 反查唯一 uid，因为多个 uid 可以合法共享同一 `{vpn,asid,vmid,s2xlate}` entry。

## 8. 参数、默认挂接和收尾检查

### 8.1 这个特性是什么

新增闭环能力不能只靠代码存在，还需要配置、默认 sequence 和收尾条件配合。否则会出现 handler 投递了 payload 但没有消费者、directed request 未完成 testcase 就结束、或者非法 plus 组合跑到中途才暴露。

这个特性不是某一个接口 flow，而是把前面几个闭环能力接入真实 testcase 的“胶水层”。例如 redirect recovery 需要 redirect sequence 默认挂接，否则 `pending_redirect_drive_q` 里有 payload 也没人 drive；flushSb directed flow 需要 plus 控制默认关闭，否则普通 smoke 会被额外扰动；PTW wait 需要 timeout 和 enable，避免所有 replay 都无条件等待 L2TLB response。参数、default sequence 和 end check 共同保证这些能力可控、可消费、可收尾。

### 8.2 相关源码修改

相关文件：

- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`
- `mem_ut/ver/ut/memblock/tc/src/tc_base.sv`
- `mem_ut/ver/ut/memblock/tc/src/tc_dispatch_real_smoke.sv`
- `mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_driver.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv`

#### 添加/修改的参数和字段

| 参数/字段 | 含义 | 为什么需要 | 在本特性中的功能 |
|---|---|---|---|
| `MEMBLOCK_REDIRECT_SEQ_EN` | 是否允许 redirect sequence 回灌 payload。 | redirect event 出现后必须有人真实 drive `io.redirect`。 | 默认开启；关闭但遇到 redirect 时 fatal。 |
| `MEMBLOCK_REDIRECT_DRIVE_TIMEOUT` | redirect sequence 等 payload/drive 的超时。 | payload 无人消费时不能静默挂住。 | redirect sequence idle loop 中定位卡死。 |
| `MEMBLOCK_REDIRECT_FREEZE_TIMEOUT` | recovery freeze 最大等待轮数。 | active redirect 长时间不完成说明回灌或 ack 异常。 | `advance_active_redirect()` 超时 fatal。 |
| `MEMBLOCK_FLUSHSB_SEQ_EN` | 是否启用 directed flushSb flow。 | 普通 smoke 默认不应额外扰动 SBuffer。 | 默认关闭；打开后 flushSb request 才生效。 |
| `MEMBLOCK_FLUSHSB_REQUEST_CYCLE` | scheduled flushSb 的目标 cycle。 | directed case 需要指定发 `flushSb` 的时机。 | LSQ commit sequence 到点转 pending。 |
| `MEMBLOCK_FLUSHSB_TIMEOUT` | 等 `sbIsEmpty=1` 的超时。 | DUT 不返回 empty 时要暴露问题。 | flushSb waiting 阶段 fatal。 |
| `MEMBLOCK_REPLAY_WAIT_PTW_EN` | 是否启用 PTW-back replay wait。 | 普通 replay 不一定需要等待 L2TLB responder。 | 默认关闭；directed PTW case 打开。 |
| `MEMBLOCK_REPLAY_WAIT_PTW_TIMEOUT` | PTW wait 的超时。 | 缺失 L2TLB response 时不能永久挂住。 | timeout 后释放 replay 并 warning。 |
| `dispatch_real_smoke_active` | 当前是否处于 real dispatch smoke。 | 某些基础 driver 在 real flow 中需要不同 idle 默认值。 | CSR driver 用它给 TLB CSR mode 可工作的默认值。 |

这些参数的默认值体现了“默认不扰动普通 smoke，必要闭环默认可消费”的原则。redirect sequence 默认开启，是因为一旦真实 DUT 给出 redirect/memoryViolation，payload 必须能回灌，否则 recovery 会 freeze；flushSb 和 PTW wait 默认关闭，是因为它们是 directed 增强，不应该影响普通随机/real smoke；timeout 默认非零，是为了把“无人消费、DUT 不返回、connect 未生效”这类问题变成可定位 fatal/warning，而不是长时间挂死。

#### 添加/修改的函数/task

```systemverilog
seq_csr_common::init();
seq_csr_common 参数检查和 clamp;
```

- 参数检查的功能：`seq_csr_common::init()` 在 sequence 使用参数前加载 plus 配置，并对危险组合做 clamp/warning。它承担的是“启动期把配置修正到可运行范围”的职责，而不是等 randomize 或 runtime 卡住后再报错。例如 flushSb sequence 打开但 timeout 为 0，会 clamp 到 1；配置了 `flushSb` request 但 LSQ commit sequence 未开启会 warning 并忽略 request；PTW wait 打开但 timeout 为 0，也会 clamp 到 1。LSQ commit 不再依赖 `max_cycles` 覆盖 request cycle，而是把 scheduled/pending `flushSb` 作为 pending work，避免到点前提前 idle 退出。
- 参数默认值的意义：redirect sequence 默认开启，因为 redirect event 一旦出现就需要消费者；flushSb 和 PTW wait 默认关闭，因为它们是 directed 增强，不应影响普通 smoke。默认值放在 `env/plus.sv` 和 `seq/plus_cfg/default.cfg` 中，实际 sequence 通过 `seq_csr_common` getter 获取，避免各个 sequence 自己散落解析 plusarg。

```systemverilog
uvm_config_db#(uvm_object_wrapper)::set(...,
    "env.u_redirect_agent_agent.sqr.main_phase",
    "default_sequence",
    memblock_redirect_dispatch_sequence::type_id::get());
```

- `tc_base.sv` 默认挂接的功能：它把 LSQENQ、issue、LSQCOMMIT、redirect、L2TLB responder 等 sequence 挂到对应 agent sequencer 的 main_phase default_sequence。它在本特性中承担“保证消费者存在”的职责。是否真正工作仍由 plus 和 sequence 内部条件控制，但类型必须默认挂好，否则 handler 投递 redirect payload、flushSb pending 或 L2TLB response 请求时，可能没有任何 sequence 在运行。

```systemverilog
memblock_sync_pkg::dispatch_real_smoke_active = 1'b1;
...
memblock_sync_pkg::dispatch_real_smoke_active = 1'b0;
```

- `dispatch_real_smoke_active` 的功能：它是 real dispatch smoke 运行期标志。实现上 sequence 进入 real smoke 时置 1，结束时清 0；CSR driver 看到该标志后把 `priv_imode/dmode` 默认驱动为 `2'd3`，避免 real dispatch flow 在没有专门 CSR testcase 配置时走到非预期 TLB/priv 模式。它是 CSR 默认驱动辅助开关，不属于 redirect、flushSb 或 PTW wait 的核心状态。

```systemverilog
function bit all_transactions_terminal_done();
function void end_test_check();
function void report_unfinished_status();
function void report_hdl_bit(input string path);
function void report_hdl_value(input string path);
```

- `all_transactions_terminal_done()` 的功能：它是 real smoke 正常结束的判定入口。当前 completion 由 `terminal_done_uid >= main_trans_num` 表示；`success` 只表示 normal pass 结果，fault/exception 可以是 `success=0 && terminal_done=1`。这样 testcase 不会因为合法 fault 终态卡住，也不会在 directed 控制任务还没闭环时提前退出。
- `end_test_check()` 的功能：它是最终一致性检查。实现上关闭 monitor capture，清理残留 raw event，并检查 active map、issue queue、redirect state、flushSb state、PTW wait queue 是否都回到 idle。如果某个闭环状态没清干净，说明前面某个 sequence、monitor 或 handler 没有完成协作。
- `report_unfinished_status()` 的功能：它在 real smoke timeout 时打印诊断信息。职责是把“没结束”的原因从单纯 timeout 变成可定位状态：哪些 uid 仍 active，哪些 queue/map 未清，redirect/flush 是否卡住，以及关键 HDL 信号如 issue ready、TLB request、dcache ready、writeback valid 是否有活动。它不改变功能行为，但减少后续 debug 对波形的依赖。
- `report_hdl_bit()` / `report_hdl_value()` 的功能：它们是 timeout debug 辅助函数，负责用 `uvm_hdl_read` 读取常见卡点信号并打印。bit 版本适合 valid/ready，value 版本适合 fuOpType、src、imm、ROB/LQ/SQ value 等多 bit 信号。它们属于可观测性增强，不参与状态机决策。

## 9. 当前支持状态

| 功能 | 当前状态 | 默认是否开启 |
|---|---|---|
| Raw monitor capture gate 和统一 raw queue | 已支持 | 随 real dispatch flow 生效 |
| `memblock_wb_event_t` 统一事件归一化 | 已支持 | 随 writeback/redirect adapter 生效 |
| active uid、issue_epoch、replay_seq 防迟到事件误更新 | 已支持 | 随 `normalize_feedback_event()` 生效 |
| memoryViolation 转 redirect event | 已支持 | 随 monitor/recovery flow 生效 |
| redirect 真实回灌 `io.redirect` | 已支持 | `MEMBLOCK_REDIRECT_SEQ_EN=1` 默认开启 |
| redirect monitor feedback 移除/禁用 | 已支持 | redirect monitor 不再反馈 dispatch recovery |
| redirect/flush 阶段化恢复 | 已支持 | 随 recovery flow 生效 |
| issue redirect 邻近拍 fired_mask | 已支持 | 随 issue driver 生效 |
| LSQENQ 等 canAccept 时的 flush abort | 已支持 | 随 LSQENQ driver 生效 |
| flushSb/sbIsEmpty directed 闭环 | 已支持 | 默认关闭，需要 `MEMBLOCK_FLUSHSB_SEQ_EN=1` |
| scheduled flushSb due 后遇 flush 保留 pending | 已支持 | directed flushSb 打开后生效 |
| PTW-back replay 等 L2TLB response done | 已支持 | 默认关闭，需要 `MEMBLOCK_REPLAY_WAIT_PTW_EN=1` |

## 10. 尚未覆盖或仍需验证

- 尚未跑 directed memoryViolation waveform 验证 `io_redirect_valid` pulse、memoryViolation -> redirect 触发路径，以及软件 flush 范围/状态一致性。
- 尚未跑 replay 后迟到 feedback 的 directed case 验证 `issue_epoch/replay_seq` 过滤是否符合预期。
- 尚未跑 directed flushSb case 验证 `flushSb` pulse 后等待 `sbIsEmpty=1`。
- 尚未跑 directed PTW-back replay case 验证等待 L2TLB response done 或 timeout 后重新入队。
- 不模拟完整前端 refetch、ROB/rename 恢复、MemBlock 内部 LoadQueueReplay、TLB miss queue、DCache MSHR 或 SBuffer 内部状态机。
- `lqCancelCnt/sqCancelCnt/ldCancel/mdpTrain/updateLFST` 不属于本轮闭环落地任务。
