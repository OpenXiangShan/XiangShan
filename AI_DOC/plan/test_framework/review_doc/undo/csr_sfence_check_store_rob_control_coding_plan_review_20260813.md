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
| control topology mode | 公共 plus `MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE` 的已校验、一次冻结快照；不由 testcase/VSEQ 写入。 | `plus.sv -> seq_csr_common -> memblock_sync_pkg` 中的 `DISABLED/AUTO_MAIN_TABLE/MANUAL_MAIN_TABLE/MANUAL_CONTROL_MAIN_TABLE`。 | mode=`2` 的普通 manual testcase 不启动 CSR/Fence control worker。 |
| runtime snapshot | CSR monitor 已从 DUT interface 观察并发布的运行时 CSR 快照。 | `memblock_sync_pkg::runtime_csr_snapshot` 与 `runtime_csr_snapshot_seq`。 | CSR action sendover 后，只接受新序号且 expected 字段匹配的 snapshot。 |
| control CSR baseline | 当前 `control_reset_epoch` ready 后由 CSR monitor 首次发布的 runtime snapshot 代际许可与最小序号。 | `memblock_sync_pkg::control_csr_runtime_baseline`。 | `CSR_CONFIG_PENDING` 在该标记到达前不能创建 token；到达后每个 action 再从 latest 一次冻结自己的当前 raw 配置，避免使用 reset 前 snapshot 或回退较早 CSR 配置。 |
| `flushSb` sendover | LSQ commit item 的 `finish_item()` 返回，表示请求已交付 driver；不是 `sbIsEmpty` 完成。 | owner 化 `flushsb_req_q` 生命周期记录。 | sendover baseline 之前的高电平 `sbIsEmpty` 不得完成新请求。 |
| C0/C4 | SFence monitor 观察到接口有效的采样点，以及既有 L2TLB adapter 实际完成失效的采样点；它们只属于 L2TLB runtime reset 代际。 | `schedule_sfence_invalidate()`、`apply_due_sfence_invalidate()` 和带 `l2tlb_reset_epoch` 的 lifecycle record。 | 同拍 C0 可由 pre-drive armed owner 记录，但要等 sendover 后才消费。 |
| L2 level hold | CSR driver 对 `flush_l2_enable=1` 的私有保持状态。 | `l2_flush_level_hold`。 | ASSERT 后 worker 可等待 RELEASE，driver idle sample 仍驱动高电平。 |

本 plan 中关键新增对象的抽象职责如下：

| 对象 | 抽象功能职责 |
|---|---|
| `memblock_control_barrier_service` | 每个 dispatch service tick 只推进当前唯一控制屏障，按 owner 消费既有 monitor/adapter 事实，不驱动 DUT 接口。 |
| `build_control_auto_main_table()` | 只为 AUTO mode 建立 `N+1` 主表、CSR/SFence 间隔预约和末尾 `check_store`；不影响 legacy random table。 |
| `drive_l2_flush_level()` | 由 CSR worker 发送 owner 化 ASSERT/RELEASE；高电平的连续保持由 driver 私有 hold 完成。 |
| `uses_control_barrier_topology()` | 只读取 plus 冻结的 mode snapshot，判断是否需要控制 worker、bootstrap、屏障 service 和 shutdown。 |

## Review 范围与源码依据

本 review 对照两份草案的抽象行为，并检查下列当前源码边界：

| 当前源码位置 | 已确认的现有行为 | 对 plan 的约束 |
|---|---|---|
| `seq/base_seq_help/memblock_dispatch_base_sequence.sv`，`build_main_table()`、`build_random_main_table()`、`import_manual_main_table()` | `MEMBLOCK_USE_MANUAL_MAIN_TABLE` 当前只选择 legacy random/import；manual import 直接建立 UID/status。 | control topology 不得由该 plus 推导；AUTO 与 direct-manual 必须分离入口。 |
| `seq/base_seq/memblock_main_dispatch_auto_build_main_table_base_sequence.sv`，`body()`、`service_real_dispatch_flow()`、`all_transactions_terminal_done()` | generic main sequence 当前统一执行主服务；当前 stop 直接经 `request_global_stop_if_done()`。 | 新控制 drain/shutdown 必须在 global stop 前收敛；mode=`0` 保持 generic legacy 建表，mode=`1` 才进入控制自动建表，manual sequence 仍复用原 service。 |
| `seq/base_seq/memblock_main_dispatch_manual_main_table_sequence.sv`，`body()` | manual sequence 继承 auto service，但自身调用 direct builder。 | MANUAL 不能因为继承关系自动成为 AUTO；其 service 不得被跳过。 |
| `seq/base_seq/memblock_lsqcommit_dispatch_base_sequence.sv`，`send_lsqcommit_cycle()` | 当前 `mark_flushsb_driven()` 在 `start_item/finish_item` 前调用。 | 必须拆分 attached 与 driver sendover，避免 queue 消费被误写成接口完成。 |
| `seq/base_seq_help/common_data_transaction.sv`，`apply_redirect_flush_range()`、`runtime_drain_complete()`、`schedule_sfence_invalidate()`、`apply_due_sfence_invalidate()` | redirect 扫描 active window；runtime drain 是 global stop 前置条件；SFence C4 由 due queue 实际消费。 | 静态控制标记要从普通 reissue 分流；C4 不能用固定拍数或 queue 为空代替。 |
| `seq/base_seq_help/lsq_commit_handler.sv`，`apply_raw_ctrl_deq()` | raw ctrl 会调用 `update_sb_is_empty()`，commit cursor 只跨过 terminal 前缀。 | 需要 immutable observation seq、control commit 分流和 control terminal 后屏障释放。 |
| `agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv`、`agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_xaction.sv` 与 `common/memblock_common/src/memblock_sync_pkg.sv` | runtime CSR snapshot 仅在 payload changed 时增加序号，且 `dispatch_raw_csr_t` 只覆盖 monitor 采集字段，不是完整 agent xaction。 | 普通 CSR 必须产生 monitor 可见变化；CSR worker 必须构造完整、未随机化的 xaction，不能把 raw snapshot 或未约束随机字段直接交给 driver。 |
| `agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_driver.sv` | 无 item 时 generic `drive_idle()` 会把 CSR 输出归零。 | `flush_l2_enable` 必须由 owner 化 driver hold 保持，不能依赖单次 ASSERT。 |
| `env/plus.sv`、`seq/base_seq_help/seq_csr_common.sv`、`tc/src/tc_base.sv`、`tc/src/basicTest.sv` | 公共 plus 在 build 阶段由 `seq_csr_common::reload_from_plus()` 解析；real-smoke 的后续 default sequence 写入会覆盖基类配置。 | mode 必须走 `plus -> seq_csr_common`，并在所有 legacy default 写入后按快照安装 control worker；不能再由 VSEQ/testcase setter 作为第二来源。 |

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
| 1 | generic main sequence 当前会受 `MEMBLOCK_USE_MANUAL_MAIN_TABLE` 影响，无法直接把“继承 auto 类”视为 AUTO。 | `build_main_table()` 只依该 plus 分支。 | 增加显式 topology mode；mode=`1` 才进入 `build_control_auto_main_table()`，mode=`0` 保留 generic legacy build，manual/control-manual direct builder 不经过 generic build。 | 是。 |
| 2 | 手工控制条目与普通 `MANUAL_MAIN_TABLE` 的 worker 拓扑矛盾。 | 普通 manual 不启动 worker，控制条目会永远停在等待状态。 | 增加 `MANUAL_CONTROL_MAIN_TABLE`：不自动追加/预约，但启动 worker、service、bootstrap 和 drain。 | 是。 |
| 3 | direct-manual testcase/VSEQ 会继承 real-smoke build/default sequence，可能误得到 AUTO worker。 | `tc_dispatch_real_mixed_wb_smoke` 和 cancel-reconcile 均复用 real dispatch 基类/服务，且 real-smoke 在 `super.build_phase()` 后会再次写 generic default。 | mode 改为公共 plus，manual/cancel preset 显式写 `2`；所有 legacy default 写入结束后按冻结 snapshot 覆盖 control worker；main sequence 对 mode/builder mismatch fail-fast，VSEQ/testcase 不再写 mode。 | 是。 |
| 4 | `L2_FLUSH_LEVEL` 同时要求 worker 连续发 high item 和 driver idle hold，会有双重 writer 语义。 | CSR driver idle 会清零；driver hold 已是保存完整 baseline 的自然所有者。 | worker 只发一次 ASSERT 与一次 RELEASE；driver 的 owner 化 `l2_flush_level_hold` 在所有 idle sample 保持 high。 | 是。 |
| 5 | 控制 topology 若继续由 testcase/VSEQ 选择，会把场景选择与公共框架参数选择混在一起，且无法通过 testcase preset/用户 plus 统一覆盖。 | `seq_csr_common` 已是公共 dispatch 参数读取入口，`VSEQ_MAIN` 只负责 factory 选择 virtual sequence。 | 新增单一 `MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE=0..3`；`plus.sv -> seq_csr_common` 范围校验后冻结到 `memblock_sync_pkg`，VSEQ 只选择 sequence，sequence 只校验自身与 mode 的匹配。 | 是。 |
| 6 | mode=`0` 的 legacy manual import 若包含控制 `op_class`，既不启动 worker 也不进入控制 service，会在控制 UID 处静默卡住。 | DISABLED generic builder 仍允许 `MEMBLOCK_USE_MANUAL_MAIN_TABLE=1` 走 `import_manual_main_table()`。 | DISABLED post-build 统一拒绝 CSR/SFence/check_store；含控制条目的手工表只能使用 mode=`3` 的 dedicated manual-control builder。 | 是。 |
| 7 | CSR runtime snapshot 被描述成可直接复制为完整 CSR xaction，容易让未被 monitor 采集的 driver 字段没有确定来源。 | `dispatch_raw_csr_t` 未覆盖完整 `csr_ctrl_agent_agent_xaction`；后者大量字段没有默认随机约束，直接 randomize 会随机改 DUT 输入。 | `configure_csr_control_xaction()` 先创建不 randomize 的完整 xaction，显式映射 raw 中可观察字段，固定 gap、`flush_l2_enable` 和其余安全字段；ASSERT/RELEASE 复用同一映射再覆盖自己的字段。 | 是。 |
| 8 | 运行中 reset 若只检查 control marker 是否已 active，会遗漏“普通 UID 已 admission、控制 UID 尚未到达”的半重建状态。 | `reset_all_tables()` 只在建表时重置主表运行期状态；现有普通 UID 的 ROB/LSQ 状态没有本专项定义的 reset/reissue 重建协议。 | 以既有 `dispatch_progress.max_enqueued_uid_valid`、active ROB map、barrier/owner 为统一启动判据；任一 UID 已 admission 后的 global/physical/L2TLB runtime reset 直接 fatal。 | 是。 |
| 9 | SFence lifecycle record 的 reset epoch 与 CSR/`sbIsEmpty`/L2 done 的控制 reset epoch 含义不同，混用会让旧 C0/C4 误匹配。 | Fence monitor 和 adapter 当前用 `l2tlb_current_reset_epoch` 标记 raw fence/event；control bootstrap 使用独立 `control_reset_epoch`。 | token 在 arm 时冻结 `l2tlb_reset_epoch_at_arm`，C0/C4 record 和 C4 完成只匹配该 epoch；`control_reset_epoch` 只用于 CSR、`sbIsEmpty` 和 L2 done。 | 是。 |
| 10 | control runtime ready 只表示 producer 已允许发布，CSR action 若立刻读取 latest 仍可能拿到 reset 前 snapshot；若所有 action 长期复用首份 baseline payload，又会把较早 CSR 配置回退。将 raw snapshot 的 changed/write pulse带入普通 CSR 或 L2 hold 也会重复驱动无关更新。 | `get_latest_runtime_csr_snapshot()` 只提供 mutable latest，没有按 seq 回读 history；CSR monitor 仅在 payload changed 时更新 snapshot，且 raw sample 可含 `satp_changed` 等脉冲。 | 首份 post-ready snapshot 仅发布 `valid/epoch/first_snapshot_seq` gate；每个 CSR/L2 action 在 gate 成立后从 latest API 一次读取并冻结自己的 current raw+seq。普通 CSR 只保留本 action 的 `satp_changed`，L2 ASSERT/RELEASE 清除所有 changed 与一次性 write/trigger valid。 | 是。 |
| 11 | L2TLB lifecycle owner 已建立后，现有 responder 仍可能处于 post-reset transport baseline 初始化；此时立即 SFence 会把正常异步准备误判为动作缺失。 | `memblock_sync_pkg::l2tlb_post_reset_baseline_done()` 已是现有 responder/release 的 ready proof。 | `SFENCE_REQ` 在 owner/topology 正确但 proof 未到时保持等待；只对 owner/topology 缺失 fatal，proof 超时使用既有控制超时诊断。 | 是。 |

## 最终主审

### 一致性检查

1. 主表生成、manual table、VSEQ 和 testcase 入口只读取 `plus -> seq_csr_common -> memblock_sync_pkg` 的唯一 mode snapshot；不再由 VSEQ/testcase 写 mode，也不以 legacy plus 或类名猜测控制 worker 拓扑。
2. 高频 dispatch service 只查询 active barrier、精确 request/event id 和 latest observation；没有引入每拍主表全扫描。
3. `flushSb`、CSR snapshot、C0/C4、L2 done 的完成事实均有 owner、baseline 或 reset epoch 保护；其中 C0/C4 固定使用 L2TLB epoch，CSR/`sbIsEmpty`/L2 done 固定使用 control epoch，避免旧 level/旧 token 或跨域 epoch 推进新动作。
4. `check_store` 的高电平保持只有 CSR driver 维护，CSR worker 只负责 item 生命周期，避免 signal 所有权重叠。
5. shutdown 先等 `control_action_drain_complete()` 和 worker ack，再允许 `runtime_drain_complete()` 打开 global stop，解除现有 stop 与 worker 退出的循环依赖。
6. 当前 `post_manual_config()` 只做 vaddr/priority 的通用一致性校验；MANUAL_CONTROL builder 以中性值满足该校验，随后由 control-neutral `validate_main_table_entry()` 负责控制字段校验，无需把普通 behavior 校验扩展到控制项。
7. mode=`0/2` 的任何建表入口都在 post-build 拒绝控制 `op_class`；只有 mode=`1/3` 可进入 control-neutral admission/retire 流程，不会因缺少 worker 形成静默等待。
8. CSR worker 的普通 action 与 `L2_FLUSH_LEVEL` action 都从完整、未随机化的 CSR xaction 开始，只将 monitor 可证明的运行时字段映射为 expected snapshot；不会把不完整 raw snapshot 误当作全量 driver baseline。
9. reset 只允许出现在控制主表尚未发生任何 UID admission 的启动窗口；任一普通或控制 UID 进入运行期后直接 fail-fast，避免做局部清理却继续使用旧 ROB/LSQ 状态。
10. CSR control action 还会等待当前 `control_reset_epoch` 的第一份 monitor runtime snapshot 作为 gate，再分别冻结自身启动时的 latest raw+seq；普通 CSR 仅驱动自己的 SATP change pulse，L2 profile 清除所有 CSR changed/write pulse，确保 level hold 只承担 L2 flush，也不会回退更早 CSR 配置。
11. SFence 在 L2TLB owner/topology 已验证后仍等待既有 post-reset baseline proof；该等待不改变 C0/C4/commit 顺序，也不会把正常 responder 初始化当成 fatal。

### 最终复审结果

第 11 轮修订后的独立复审结论为 PASS，未发现必须修改项。最终主审复核确认：`MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE` 是唯一 mode 输入，VSEQ 不再写 mode；generic/manual/manual-control builder、worker default 覆盖、reset/bootstrap、CSR runtime snapshot、SFence C0/C4 和 `check_store` L2 flush 均有一致的所有权与完成边界。新增的 current-epoch CSR gate、per-action latest freeze、L2TLB/control epoch 分离、post-reset baseline 等待和 admission 后 reset fail-fast 均不改变两份草案的主体时序，只消除当前框架中的不确定字段、旧 snapshot、CSR 配置回退、旧 CSR pulse 与半重建路径。

## 未完成验证与后续风险

本轮没有 coding，也没有运行编译或仿真，因此以下项目仍需在按 plan coding 后验证：

1. AUTO mode=`1`：`N+1`、CSR/SFence 重合优先级、尾部 `check_store`、worker shutdown。
2. MANUAL mode=`2`：mixed/manual 与 cancel-reconcile 保持 legacy builder/default sequence，且拒绝 control `op_class`。
3. MANUAL_CONTROL mode=`3`：显式控制条目可完成 bootstrap、屏障、commit 和 retire。
4. SFence：`flushSb` sendover 后的新鲜 `sbIsEmpty`、同拍 C0、C4 effective 后才允许 commit。
5. `check_store`：ASSERT 到 RELEASE 期间 CSR driver 每个 idle sample 都保持 `flush_l2_enable=1`，随后完成 done-high、RELEASE、done-low 闭环。
6. 参数路径：默认 cfg mode=`0` 保持 legacy；AUTO、manual、manual-control preset 和命令行 `plus_arg` 选择值后，必须与所启动的 main sequence 匹配，非法值及 mismatch 都命中预期 `uvm_fatal`。
7. CSR control：普通 CSR、ASSERT 与 RELEASE 都使用完整、未随机化的 xaction；monitor runtime snapshot 只验证当前已映射的字段，不把未采集字段误写入 snapshot 语义。
8. reset：首笔 UID admission 前的 bootstrap/reset handshake 可完成；任一 UID admission 后的 global/physical/L2TLB runtime reset 必须命中预期 `uvm_fatal`，直到后续另有全主表重建协议。
9. bootstrap：`control_runtime_ready` 后必须由 CSR monitor 发布当前 control epoch 的 `control_csr_runtime_baseline.first_snapshot_seq`；CSR action 不得抢在该 gate 前创建，且每个 action 只能在启动时从 latest 一次冻结自己的 raw+seq，普通 CSR 只保留自身 SATP change pulse，L2 profile 不得保持任何 CSR changed/write pulse。
10. SFence：L2TLB lifecycle owner/topology 与 post-reset baseline proof 均到位后才 arm/drive；前者缺失为 fatal，后者未到只等待到控制超时。

建议按 coding plan 中的 directed 验收表执行远端 V2 编译与仿真；在这些验证完成前，本文不能作为功能已验证的证明。
