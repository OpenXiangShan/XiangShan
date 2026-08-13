# CSR/SFence/`check_store` ROB 控制屏障 Coding Plan Review

| 项目 | 内容 |
|---|---|
| 状态 | Plan review 通过，尚未 coding |
| 日期 | 2026-08-13 |
| 适用版本 | `mem_ut_uvm_v2` |
| 被审对象 | `AI_DOC/plan/test_framework/plan/undo/csr_sfence_check_store_rob_control_coding_plan_20260813.md` |
| 草案依据 | `AI_DOC/analysis/framework_design/csr_sfence_rob_control_barrier_flow_draft_20260813.md`、`AI_DOC/analysis/framework_design/check_store_rob_flush_l2_flow_draft_20260813.md` |
| 本轮实现状态 | 未修改 SystemVerilog、cfg、Makefile、RTL 或脚本；未执行编译和仿真。 |

本 review 只审查可 coding plan 与当前 V2 测试框架的匹配关系，不是已实现代码的功能 review。结论中的“通过”表示 plan 已没有必须修改项，不表示功能已经编译或仿真验证。

## 术语与抽象功能说明

| 术语 | 当前含义 | 对应对象或落点 | 示例 |
|---|---|---|---|
| 控制标记 | 占用 UID 和连续 `robIdx`、但不进入 LSQ/issue 的 CSR、SFence 或 `check_store` 主表项。 | `memblock_op_class_e`、`status_transaction`、`uid_by_active_rob`。 | UID 10 是 SFence 时，它能成为 modeled ROB head，但没有 LQ/SQ 映射。 |
| 静态屏障 | 控制标记已 control-active、尚未开始接口动作的等待阶段。 | `WAIT_OLDER_ROB_COMMIT` 与 `active_control_barrier_uid`。 | UID 8 redirect 到 UID 5 时，UID 10 保留等待，UID 5..9 重新执行。 |
| action owner | 一次实际控制动作的唯一身份。 | `uid + dynamic_epoch + action_generation + kind`。 | 旧 `sbIsEmpty` 或 L2 done 不能完成新的 `check_store` 动作。 |
| control topology mode | testcase 或明确 VSEQ allowlist 写入的控制 worker/建表拓扑。 | `memblock_sync_pkg` 中的 `DISABLED/AUTO_MAIN_TABLE/MANUAL_MAIN_TABLE/MANUAL_CONTROL_MAIN_TABLE`。 | 普通 manual testcase 为 MANUAL，不启动 CSR/Fence control worker。 |
| runtime snapshot | CSR monitor 已从 DUT interface 观察并发布的运行时 CSR 快照。 | `memblock_sync_pkg::runtime_csr_snapshot` 与 `runtime_csr_snapshot_seq`。 | CSR action sendover 后，只接受新序号且 expected 字段匹配的 snapshot。 |
| `flushSb` sendover | LSQ commit item 的 `finish_item()` 返回，表示请求已交付 driver；不是 `sbIsEmpty` 完成。 | owner 化 `flushsb_req_q` 生命周期记录。 | sendover baseline 之前的高电平 `sbIsEmpty` 不得完成新请求。 |
| C0/C4 | SFence monitor 观察到接口有效的采样点，以及既有 L2TLB adapter 实际完成失效的采样点。 | `schedule_sfence_invalidate()`、`apply_due_sfence_invalidate()` 和 lifecycle record。 | 同拍 C0 可由 pre-drive armed owner 记录，但要等 sendover 后才消费。 |
| L2 level hold | CSR driver 对 `flush_l2_enable=1` 的私有保持状态。 | `l2_flush_level_hold`。 | ASSERT 后 worker 可等待 RELEASE，driver idle sample 仍驱动高电平。 |

本 plan 中关键新增对象的抽象职责如下：

| 对象 | 抽象功能职责 |
|---|---|
| `memblock_control_barrier_service` | 每个 dispatch service tick 只推进当前唯一控制屏障，按 owner 消费既有 monitor/adapter 事实，不驱动 DUT 接口。 |
| `build_control_auto_main_table()` | 只为 AUTO mode 建立 `N+1` 主表、CSR/SFence 间隔预约和末尾 `check_store`；不影响 legacy random table。 |
| `drive_l2_flush_level()` | 由 CSR worker 发送 owner 化 ASSERT/RELEASE；高电平的连续保持由 driver 私有 hold 完成。 |
| `uses_control_barrier_topology()` | 只读取 testcase/VSEQ 显式 mode，判断是否需要控制 worker、bootstrap、屏障 service 和 shutdown。 |

## Review 范围与源码依据

本 review 对照两份草案的抽象行为，并检查下列当前源码边界：

| 当前源码位置 | 已确认的现有行为 | 对 plan 的约束 |
|---|---|---|
| `seq/base_seq_help/memblock_dispatch_base_sequence.sv`，`build_main_table()`、`build_random_main_table()`、`import_manual_main_table()` | `MEMBLOCK_USE_MANUAL_MAIN_TABLE` 当前只选择 legacy random/import；manual import 直接建立 UID/status。 | control topology 不得由该 plus 推导；AUTO 与 direct-manual 必须分离入口。 |
| `seq/base_seq/memblock_main_dispatch_auto_build_main_table_base_sequence.sv`，`body()`、`service_real_dispatch_flow()`、`all_transactions_terminal_done()` | auto main sequence 统一执行主服务；当前 stop 直接经 `request_global_stop_if_done()`。 | 新控制 drain/shutdown 必须在 global stop 前收敛，且 manual sequence 仍复用原 service。 |
| `seq/base_seq/memblock_main_dispatch_manual_main_table_sequence.sv`，`body()` | manual sequence 继承 auto service，但自身调用 direct builder。 | MANUAL 不能因为继承关系自动成为 AUTO；其 service 不得被跳过。 |
| `seq/base_seq/memblock_lsqcommit_dispatch_base_sequence.sv`，`send_lsqcommit_cycle()` | 当前 `mark_flushsb_driven()` 在 `start_item/finish_item` 前调用。 | 必须拆分 attached 与 driver sendover，避免 queue 消费被误写成接口完成。 |
| `seq/base_seq_help/common_data_transaction.sv`，`apply_redirect_flush_range()`、`runtime_drain_complete()`、`schedule_sfence_invalidate()`、`apply_due_sfence_invalidate()` | redirect 扫描 active window；runtime drain 是 global stop 前置条件；SFence C4 由 due queue 实际消费。 | 静态控制标记要从普通 reissue 分流；C4 不能用固定拍数或 queue 为空代替。 |
| `seq/base_seq_help/lsq_commit_handler.sv`，`apply_raw_ctrl_deq()` | raw ctrl 会调用 `update_sb_is_empty()`，commit cursor 只跨过 terminal 前缀。 | 需要 immutable observation seq、control commit 分流和 control terminal 后屏障释放。 |
| `agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv` 与 `common/memblock_common/src/memblock_sync_pkg.sv` | runtime CSR snapshot 仅在 payload changed 时增加序号。 | 普通 CSR 必须产生 monitor 可见变化；不得把发送 xaction 当作 snapshot。 |
| `agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_driver.sv` | 无 item 时 generic `drive_idle()` 会把 CSR 输出归零。 | `flush_l2_enable` 必须由 owner 化 driver hold 保持，不能依赖单次 ASSERT。 |

## 草案对齐结论

| 草案约束 | Coding plan 对应设计 | Review 结论 |
|---|---|---|
| CSR/SFence/`check_store` 有连续 `robIdx`，不进 LSQ/issue。 | control-neutral 主表校验、`activate_control_uid()`、issue/LSQ/behavior 早退。 | 对齐。 |
| 控制标记等前序访存连续提交后才执行，并在自身 `terminal_done` 后解除屏障。 | `WAIT_OLDER_ROB_COMMIT` 以 `commit_cursor_uid==uid` 启动动作；`CONTROL_COMMIT_READY -> rob_commit -> terminal_done` 后清 barrier。 | 对齐。 |
| CSR 完成凭 UT monitor runtime snapshot，而非已发送 xaction。 | `RUNTIME_CSR_SNAPSHOT` 使用 sendover 前 baseline、新序号、expected 匹配和 monitor snapshot 归档。 | 对齐。 |
| SFence 先 `flushSb`、再 `sbIsEmpty`、再发接口、最后等 C0/C4 生效。 | owner 化 flushSb attached/sendover/completed、C0 armed、C4 lifecycle record。 | 对齐。 |
| `check_store` 在自动主表末尾，先清 SBuffer，再 L2Cache flush high/done/release/done-low。 | AUTO `N+1` 建表；`CHECK_STORE_*` 状态和 `L2_FLUSH_LEVEL` ASSERT/hold/RELEASE 闭环。 | 对齐。 |
| CSR/SFence 各有 enable/min/max，越界放弃，重合时 CSR 优先。 | 两个独立预约计划、`[0,N)` 边界、CSR 优先并重新预约 SFence。 | 对齐。 |
| 静态等待中的老 UID redirect 可以恢复；已开始控制动作被 redirect 覆盖为非法。 | 静态 marker 保留且不参加 `oldest_flushed_uid`；动作 owner 覆盖时 `uvm_fatal`。 | 对齐。 |

未发现需要改变草案主体逻辑的问题。下面的调整均为使草案能在当前框架中稳定实现的最小实现细化。

## 多轮问题与解法

| 轮次 | 发现的问题 | 源码事实 | 最小且不改变功能的解法 | 已落入 plan |
|---|---|---|---|---|
| 1 | SFence 的 C0 可能与 driver 同拍，若仅等待 `SFENCE_SENDOVER` 会漏事件。 | Fence monitor 可在接口交付采样点产生 raw SFence；sendover 在 `finish_item()` 后才记录。 | `start_item()` 前冻结 event baseline 并置 `sfence_c0_match_armed`；adapter 接受 armed/sendover owner，service 仅在 sendover 后消费 C0。 | 是。 |
| 1 | bootstrap 若放在建表前，`reset_all_tables()` 会清掉相关运行期状态。 | auto/random 和 manual import 均通过 `reset_all_tables()` 重建主表。 | bootstrap 放在 control 主表 post-build hook；epoch/request/ready 与 producer/driver ack 放 `memblock_sync_pkg`，并明确建表不清该生命周期状态。 | 是。 |
| 1 | auto main sequence 当前会受 `MEMBLOCK_USE_MANUAL_MAIN_TABLE` 影响，无法直接把“继承 auto 类”视为 AUTO。 | `build_main_table()` 只依该 plus 分支。 | 增加显式 topology mode；AUTO 新建 `build_control_auto_main_table()`，manual/control-manual direct builder 不经过 generic build。 | 是。 |
| 2 | 手工控制条目与普通 `MANUAL_MAIN_TABLE` 的 worker 拓扑矛盾。 | 普通 manual 不启动 worker，控制条目会永远停在等待状态。 | 增加 `MANUAL_CONTROL_MAIN_TABLE`：不自动追加/预约，但启动 worker、service、bootstrap 和 drain。 | 是。 |
| 3 | direct-manual testcase/VSEQ 会继承 real-smoke build/default sequence，可能误得到 AUTO worker。 | `tc_dispatch_real_mixed_wb_smoke` 和 cancel-reconcile 均复用 real dispatch 基类/服务。 | `tc_base` virtual mode getter 默认 DISABLED；mixed/manual 和 cancel-reconcile 显式 MANUAL；`basicTest` 使用明确 VSEQ allowlist；default-sequence 覆盖按 mode 条件执行。 | 是。 |
| 4 | `L2_FLUSH_LEVEL` 同时要求 worker 连续发 high item 和 driver idle hold，会有双重 writer 语义。 | CSR driver idle 会清零；driver hold 已是保存完整 baseline 的自然所有者。 | worker 只发一次 ASSERT 与一次 RELEASE；driver 的 owner 化 `l2_flush_level_hold` 在所有 idle sample 保持 high。 | 是。 |

## 最终主审

### 一致性检查

1. 主表生成、manual table、VSEQ 和 testcase 入口已有唯一 mode 来源，不再以 plus 或类名猜测控制 worker 拓扑。
2. 高频 dispatch service 只查询 active barrier、精确 request/event id 和 latest observation；没有引入每拍主表全扫描。
3. `flushSb`、CSR snapshot、C0/C4、L2 done 的完成事实均有 owner、baseline 或 reset epoch 保护，避免旧 level/旧 token 推进新动作。
4. `check_store` 的高电平保持只有 CSR driver 维护，CSR worker 只负责 item 生命周期，避免 signal 所有权重叠。
5. shutdown 先等 `control_action_drain_complete()` 和 worker ack，再允许 `runtime_drain_complete()` 打开 global stop，解除现有 stop 与 worker 退出的循环依赖。
6. 当前 `post_manual_config()` 只做 vaddr/priority 的通用一致性校验；MANUAL_CONTROL builder 以中性值满足该校验，随后由 control-neutral `validate_main_table_entry()` 负责控制字段校验，无需把普通 behavior 校验扩展到控制项。

### 最终复审结果

最后一轮独立 review 结论为 PASS，未发现必须修改项。主审复核后同意该结论：coding plan 与两份草案对齐，新增的 topology mode、owner 化完成记录、driver hold 和 stop 收敛均为当前测试框架所需的最小实现细化，不改变草案主体语义。

## 未完成验证与后续风险

本轮没有 coding，也没有运行编译或仿真，因此以下项目仍需在按 plan coding 后验证：

1. AUTO：`N+1`、CSR/SFence 重合优先级、尾部 `check_store`、worker shutdown。
2. MANUAL：mixed/manual 与 cancel-reconcile 保持 legacy builder/default sequence，且拒绝 control `op_class`。
3. MANUAL_CONTROL：显式控制条目可完成 bootstrap、屏障、commit 和 retire。
4. SFence：`flushSb` sendover 后的新鲜 `sbIsEmpty`、同拍 C0、C4 effective 后才允许 commit。
5. `check_store`：ASSERT 到 RELEASE 期间 CSR driver 每个 idle sample 都保持 `flush_l2_enable=1`，随后完成 done-high、RELEASE、done-low 闭环。

建议按 coding plan 中的 directed 验收表执行远端 V2 编译与仿真；在这些验证完成前，本文不能作为功能已验证的证明。
