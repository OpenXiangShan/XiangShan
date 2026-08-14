# CSR/SFence/`check_store` ROB 控制屏障可 Coding 方案

| 项目 | 内容 |
|---|---|
| 状态 | coding 已完成，implementation review 通过，已归档 |
| 日期 | 2026-08-13 |
| 适用版本 | `mem_ut_uvm_v2` |
| 输入草案 | `AI_DOC/analysis/framework_design/csr_sfence_rob_control_barrier_flow_draft_20260813.md`、`AI_DOC/analysis/framework_design/check_store_rob_flush_l2_flow_draft_20260813.md` |
| 本文定位 | 将两份草案合并为可实施的测试框架 flow；不修改 DUT、RM、checker 或 coverage。 |

本文以两个草案已经确认的主体语义为准：CSR/SFence 是主表中的控制标记，`check_store` 是自动主表最后一个控制标记；三者都占用连续 `robIdx`，但都不进入普通 LSQ/issue 流程。本文只规定首版需要落地的测试框架行为和边界，后续具体 CSR/SFence payload 扩展只进入本文固定的配置函数，不改变控制屏障协议。

实施状态说明：2026-08-14 已完成本 plan 的代码实现与专项 smoke。控制 worker 最终采用本文 `IMPLEMENTATION_DELTA` 规定的第二种启动方式，即仅由 `basicTest` 的专项 VSEQ 显式启动；本正文中任何仍称为 phase `default_sequence` 的历史描述均由该 delta 覆盖，不构成当前实现行为。

## 专有名词与对象职责

| 名词 | 本文含义与实现落点 | 示例 |
|---|---|---|
| 普通区间 | 自动主表中 UID `[0, N)` 的固定长度区间；`N=MEMBLOCK_MAIN_TRANS_NUM`。其中某些 slot 可被 CSR/SFence 标记替换。 | `N=100` 时普通区间为 UID `0..99`。 |
| 总主表长度 | 自动主表的实际 `data.main_trans_num`，固定为 `N+1`。 | `N=100` 时实际长度为 101。 |
| `check_store` 保留位 | 自动主表最后一个 UID，固定为 `N`，其 `op_class=MEMBLOCK_OP_CLASS_CHECK_STORE`。CSR/SFence 随机计划不得占用该 UID。 | `N=10000` 时 UID `10000` 是第 10001 条表项。 |
| 控制标记 | `CSR_CONTROL`、`SFENCE_CONTROL` 或 `CHECK_STORE` 三类不进入 LSQ/issue 的主表条目。 | UID 15 被 CSR 预约后，UID 15 不再生成普通 load/store。 |
| control-active | 控制标记已进入 `uid_by_active_rob`，可被当前 modeled ROB head/commit cursor 识别；但没有 LQ/SQ map、LSQ reservation 或 issue queue 项。 | 控制 UID 10 可成为 ROB head，但没有 `lqIdx`/`sqIdx`。 |
| control topology mode | 公共 plus `MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE` 在 testcase build 前经 `plus.sv -> seq_csr_common` 解析并冻结的主表构建和控制 worker 拓扑。它不是从 testcase 类名、`VSEQ_MAIN`、`MEMBLOCK_USE_MANUAL_MAIN_TABLE` 或其他运行期现象推测的临时判断；值 `0/1/2/3` 分别表示 `DISABLED`、`AUTO_MAIN_TABLE`、`MANUAL_MAIN_TABLE`、`MANUAL_CONTROL_MAIN_TABLE`。 | `AUTO_MAIN_TABLE` 自动生成 `N+1` 表；`MANUAL_CONTROL_MAIN_TABLE` 允许手工表显式放控制标记。 |
| 静态等待屏障 | 控制标记处于 `WAIT_OLDER_ROB_COMMIT` 的阶段。它阻止年轻 UID admission，但尚未绑定 action owner，也不属于普通 redirect/reissue 实例。 | UID 10 等 UID 0..9 连续 terminal done。 |
| action owner | 已开始实际控制动作的唯一身份，至少为 `uid + dynamic_epoch + action_generation`。 | 同一 UID 的旧 token 不能完成 redirect/reset 后的新动作。 |
| action token | 放入 CSR 或 SFence 持久 queue 的工作项。event 仅唤醒 worker，token 才是动作存在和所有权依据。 | `csr_control_action_q` 中的一项 CSR 配置动作。 |
| completion profile | 控制动作的完成事实种类。首版只有 `RUNTIME_CSR_SNAPSHOT` 与 `L2_FLUSH_LEVEL`。 | 普通 CSR 等 runtime snapshot；`check_store` 等 L2 done high/low。 |
| `flushSb` attached | 请求已从 `flushsb_req_q` 被唯一 LSQ commit consumer 取走，并附加到待发送 `lsqcommit` xaction。它不是 driver 已送出、更不是 DUT monitor 已确认 pulse。 | attached 后请求仍处于 `WAIT_FLUSHSB_REQ`，不能消费 `sbIsEmpty`。 |
| `flushSb` sendover | LSQ commit sequence 的 `finish_item()` 已由 driver 的 `item_done()` 返回，表示本次 `flushSb=1` 已交付到 driver interface。它仍不等于 monitor 对 DUT 结果的确认。 | sendover 后才进入 `WAIT_SB_EMPTY`，随后只接受 owner 化且新于 sendover baseline 的 `sbIsEmpty=1`。 |
| C0/C4 | SFence monitor 观察到 `sfence.valid` 的 sample 为 C0；既有 L2TLB adapter 对应 lifecycle event 完成生效的 sample 为 C4。两者都绑定当前 `l2tlb_current_reset_epoch`，不能与本专项的 `control_reset_epoch` 混用。V2 延迟常量为 `MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES=4`。 | C0 后不能立即 release admission。 |
| done observation | DCache monitor 发布的 `io_l2_flush_done` 最新采样，含每次采样递增的序号。它不携带 UID，也不直接写 `status_transaction`。 | `check_store` 只消费 assert 之后的首个 high 与 release 之后的首个 low。 |
| control-runtime producer ack | CSR、DCache、ctrl monitor 观察到当前 `control_reset_request` 后各自首个有效采样发布的代际确认。它适用于初始 bootstrap 与后续 reset，只证明 producer 已进入当前 `control_reset_epoch`，不能作为 CSR snapshot、`sbIsEmpty` 或 L2 done 完成事实。 | 三条 ack 与 CSR driver ack 齐备后才打开 `control_runtime_ready`；下一采样起才发布可消费 observation。 |
| control CSR baseline | CSR monitor 在当前 `control_reset_epoch` 的 ready 后首次发布 runtime snapshot 的有效性与最小序号。它只证明 global latest 已属于当前 control epoch，不是任何 action 长期复用的 CSR payload。 | ready 打开但 baseline 尚未到达时，`CSR_CONFIG_PENDING` 保持等待；到达后每个 CSR/L2 action 仍单独冻结启动时的 latest snapshot。 |
| C0 armed | SFence worker 已在把 item 交给 driver 前冻结 event baseline 并登记当前 owner/payload 的 C0 匹配资格。它允许 adapter 接住 driver/monitor 同拍产生的 C0，但本身不表示接口已交付。 | C0 可以先被记录；`finish_item()` 后的 `SFENCE_SENDOVER` 才允许 service 消费该 record。 |

以下函数名在首版固定，后续专项只能在其配置入口内扩展：

| 对象 | 抽象功能职责 |
|---|---|
| `memblock_control_barrier_service` | 每个 dispatch service tick 只推进唯一 active 控制屏障；消费 monitor/adapter 的原始完成事实并更新控制状态。它不扫描整个主表。 |
| `enqueue_csr_control_action()` | 为已越过前序提交边界的 CSR 标记写入持久 token，再触发 CSR worker 唤醒事件。 |
| `enqueue_control_flushsb_request()` | 为 SFence 或 `check_store` 标记向既有 `flushsb_req_q` 入队带 owner 的请求；不创建新的 driver、monitor 或 event consumer。 |
| `enqueue_sfence_control_action()` | 在匹配的 `sbIsEmpty` 完成后，向 SFence action queue 写 token 并触发 worker。 |
| `configure_csr_control_xaction()` | 仅按 token 构造 CSR xaction、短生命周期 expected runtime 字段与 completion profile；不等待、不驱动、不更新完成状态。 |
| `drive_csr_control_xaction()` | 仅执行 `start_item/finish_item` 并通过受 owner 校验的公共 helper 记录接口交付结束；不得把 sendover 当作 monitor 完成。 |
| `configure_sfence_control_xaction()` | 仅按 token 构造 fence xaction 和 monitor 匹配字段；不直接操作 L2TLB 状态。 |
| `drive_sfence_control_xaction()` | 仅执行 `start_item/finish_item` 并记录 SFence 接口交付结束；C0/C4 仍由 monitor/adapter 完成。 |

## 实施范围与现有复用边界

首版复用以下已存在的 V2 框架能力：

| 已有能力 | 首版复用方式 | 不改变的责任边界 |
|---|---|---|
| UID、`robIdx` 和 `commit_cursor_uid` | 控制条目使用同一 UID/ROB 分配器；前序完成以 `lsq_commit_handler.commit_cursor_uid` 证明。 | 不新建第二套 ROB、也不全表扫描前序 UID。 |
| `uid_by_active_rob` 与 retire 主路径 | 控制条目以 control-active 身份进入 ROB map，最终仍经 `rob_commit -> terminal_done -> retire_active_uid`。 | 不进入 `uid_by_lq`、`uid_by_sq`、LSQ reserve 或 issue queue。 |
| `flushsb_req_q` 与 LSQ commit sequence | CSR/SFence/`check_store` 只作为 producer 入队；`memblock_lsqcommit_dispatch_base_sequence` 仍是唯一 consumer。 | 不新增 `flushSb` agent、第二个 driver 或直接监听 `sbIsEmpty` 的线程。 |
| ctrl monitor/raw/deferred service/`update_sb_is_empty()` | 继续由现有链路采样和消费 `io_mem_to_ooo_sbIsEmpty`。 | 控制 service 只读取带 owner 的完成记录，不读 DUT pin。 |
| CSR monitor runtime snapshot | 继续由 `csr_ctrl_agent_agent_monitor` 调用 `publish_runtime_csr_snapshot()` 发布 runtime latest。 | status 保存的是 monitor runtime snapshot，绝不保存已发送 xaction 的副本。 |
| Fence monitor 与 L2TLB adapter | Fence monitor 继续发布 raw SFence；adapter 继续通过 `schedule_sfence_invalidate()` 和 `apply_due_sfence_invalidate()` 处理 C0/C4。 | 不创建第二条 L2TLB flush queue 或自行按固定拍数猜测完成。 |
| DCache L2 flush responder | 继续使用已有 `IDLE -> DRAIN -> PROBE -> DONE` 状态机和 `io_l2_flush_done`。 | 不复制 Probe、cache line、memory responder 或 backend bypass completion。 |

本实现不把 CSR/SFence/`check_store` 伪装成普通 load/store，也不扩展普通访存 `memblock_op_behavior_util::derive_op_behavior()` 的 `fuType` 语义。所有进入该 helper 的控制条目都是实现 bug，应在其调用前被 control 分流。

## 自动主表长度、UID 预约与末尾保留规则

### 固定表长定义

令 `N = seq_csr_common::get_main_trans_num()`。自动建表必须严格按以下规则执行：

```text
普通区间：                    UID 0 .. N-1，共 N 个固定 slot
末尾 check_store 保留位：      UID N
自动表实际长度：               N + 1
data.main_trans_num：          N + 1
```

因此，当 CSR/SFence 同时启用时，CSR/SFence 只可预约 `[0, N)` 中的 slot，绝不预约、截断或挪动到 UID `N`。`check_store` 永远由最后一次 `alloc_uid()` 生成，且与 UID `N-1` 使用相邻的 `robIdx`。

这里的 `N` 是“末尾 `check_store` 之前的固定表位数”。若控制随机命中某个 slot，该 slot 从普通访存变为控制标记，故真实普通访存条数会相应减少。这是同时满足“固定 UID `N` 为 `check_store`”和“不为控制标记扩展表长度”的唯一语义；日志、dump 和 testcase 名称不得再把启用控制后的 `N` 误写成“保证生成 N 笔普通访存”。若未来需要保证 `N` 笔真实普通访存，必须另行设计会随控制个数增长的总表长度，不能隐式改变本方案。

手工主表保持原语义：不自动追加 `check_store`，不自动应用 CSR/SFence 间隔预约。普通手工表以 `+MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE=2` 选择 `MANUAL_MAIN_TABLE`，其中出现控制 `op_class` 直接 `uvm_fatal`；directed testcase 如需控制条目，必须以值 `3` 选择 `MANUAL_CONTROL_MAIN_TABLE`、构造三种 `op_class` 和连续 `robIdx`，从而启动完整 control worker/barrier/bootstrap flow。

### 主表构建入口与 legacy 手工 plus 的边界

`MEMBLOCK_USE_MANUAL_MAIN_TABLE` 是现有 `memblock_dispatch_base_sequence::build_main_table()` 的 legacy 建表选择开关：当前实现以它选择 `import_manual_main_table()` 或 `build_random_main_table()`。它**不是**控制 worker 拓扑、控制标记语义或手工控制表授权来源；本专项不得再用该 plus 推导或覆盖 `control topology mode`。

首版按下列唯一入口分流，避免 direct-manual testcase 被错误要求设置自动表 plus：

```text
AUTO_MAIN_TABLE：
  只能通过 generic build_main_table() 建表。
  该入口先要求 MEMBLOCK_USE_MANUAL_MAIN_TABLE=0；若为 1 直接 uvm_fatal，
  再调用新增 build_control_auto_main_table(N)，生成 N+1、CSR/SFence 预约和末尾 check_store。

DISABLED：
  generic build_main_table() 保持既有 legacy 行为：plus 为 1 时导入既有普通手工表，
  plus 为 0 时调用既有普通随机建表；两条路径都不注入控制标记、不追加 check_store、
  不启动 control worker。post-build 发现任一 CSR/SFence/check_store op_class 立即 uvm_fatal；
  带控制条目的 legacy manual import 必须改用 MANUAL_CONTROL_MAIN_TABLE。

MANUAL_MAIN_TABLE：
  只能由 direct manual builder 调用 clear/set/import_manual_main_table() 建表；
  不调用 generic build_main_table()，不读取也不校验 MEMBLOCK_USE_MANUAL_MAIN_TABLE。
  post-build 校验发现任一 CSR/SFence/check_store op_class 立即 uvm_fatal。

MANUAL_CONTROL_MAIN_TABLE：
  只能由 direct manual-control builder 显式构造并 import 主表；
  不调用 generic build_main_table()，不读取也不校验 MEMBLOCK_USE_MANUAL_MAIN_TABLE，
  也不自动预约 CSR/SFence 或追加 check_store。post-build 至少要求存在一个控制标记，
  再启动 control worker、barrier service 和 bootstrap。
```

`build_control_auto_main_table()` 是本专项新增的自动控制建表 helper；既有 `build_random_main_table()` 保留为 `DISABLED` legacy random path，避免把 `N+1` 和 `check_store` 隐式施加到无关 testcase。任一 direct builder 误调用 generic `build_main_table()`，或 `AUTO_MAIN_TABLE` 误进入 legacy manual 分支，均直接 `uvm_fatal`，不得根据 plus 静默切换 mode。

### 控制拓扑 plus 参数、间隔 plus 参数和建表校验

新增一个互斥的公共控制拓扑 plus 参数。它是四个 mode 的唯一配置源，默认 `0`，以保证所有既有 testcase 在未更新 cfg 时保持无控制专项的 legacy 行为：

| plus 参数 | 类型 | 取值 | 语义 |
|---|---|---|---|
| `MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE` | `int` | `0` | `DISABLED`：不启动 control worker、barrier service、bootstrap 或 control drain；generic main sequence 保持既有 random/manual legacy 建表语义。 |
| `MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE` | `int` | `1` | `AUTO_MAIN_TABLE`：只允许 generic auto main sequence；生成 `N+1`、预约 CSR/SFence、末尾固定 `check_store`，并启动完整控制拓扑。 |
| `MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE` | `int` | `2` | `MANUAL_MAIN_TABLE`：只允许普通 direct-manual/cancel-reconcile main sequence；拒绝控制 `op_class`，不启动控制拓扑。 |
| `MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE` | `int` | `3` | `MANUAL_CONTROL_MAIN_TABLE`：只允许专项 direct manual-control main sequence；手工构造控制条目并启动完整控制拓扑。 |

`plus.sv` 以 `int` 读取该参数；`seq_csr_common` 保留 signed raw 值，新增纯校验 `check_control_worker_topology_mode()` 并由 `validate_and_clamp()` 调用，对范围 `[0:3]` 做 fail-fast 校验而不做 clamp；随后 `get_control_worker_topology_mode()` 返回已校验的枚举值。公共初始化 helper 在 build/config 阶段从该 getter 向 `memblock_sync_pkg` 写入一次不可变 mode snapshot；后续 testcase、VSEQ、main sequence、worker 和 service 只读该 snapshot，任何第二写者或运行中改写均 `uvm_fatal`。不采用四个独立 bit plus，避免出现多个 mode 同时为 1 的冲突配置。

新增六个公共间隔 plus 参数，默认都不生成 CSR/SFence 标记：

| 类型 | 使能 | 最小间隔 | 最大间隔 |
|---|---|---|---|
| CSR | `MEMBLOCK_CSR_CONTROL_ENABLE` | `MEMBLOCK_CSR_CONTROL_MIN_INTERVAL` | `MEMBLOCK_CSR_CONTROL_MAX_INTERVAL` |
| SFence | `MEMBLOCK_SFENCE_CONTROL_ENABLE` | `MEMBLOCK_SFENCE_CONTROL_MIN_INTERVAL` | `MEMBLOCK_SFENCE_CONTROL_MAX_INTERVAL` |

参数规则：

1. `enable=0` 时对应 min/max 不参与预约。
2. `enable=1` 时必须满足 `min_interval >= 1` 且 `max_interval >= min_interval`；`build_control_auto_main_table()` 在 AUTO 路径开始前调用公共校验 helper，非法配置直接 `uvm_fatal`。MANUAL_CONTROL 不消费这六个参数，不能因无关 interval 值被拒绝。
3. `N` 仍必须非零；自动表总长度 `N+1` 溢出时直接 `uvm_fatal`。
4. `MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE` 与这六个 key 必须同步进入 `env/plus.sv`、`seq_csr_common.sv` 的静态字段/读取/校验/getter，以及 `seq/plus_cfg/default.cfg`。默认 cfg 固定写 mode=`0`。现有 direct-manual/cancel-reconcile preset 必须显式写 mode=`2`；新增 AUTO 和 MANUAL_CONTROL 专项 preset 分别写 mode=`1` 与 mode=`3`。用户命令行 `plus_arg` 仍可按既有优先级覆盖 preset。
5. mode 只选择控制主表/worker 拓扑；`VSEQ_MAIN`、testcase 类和 sequence 类只选择场景与 main sequence，不能写入或覆盖 mode。若所选 main sequence 与 mode 不匹配，必须在 body 入口 `uvm_fatal`，不能隐式改 mode、切换 builder 或回退为其他模式。
6. 不新增 `check_store` 使能开关。自动主表无条件保留末尾 `check_store`；手工表无条件不追加。

### 双计划预约算法

建表期为 CSR、SFence 分别维护独立计划：`enabled`、`base_uid`、`next_uid`。两类计划均从初始基准 UID 0 采样一个闭区间间隔，得到首个目标；每次命中后以当前 UID 为新基准重新采样。

```text
build_control_auto_main_table(N):
  total_num = N + 1
  reset_all_tables(total_num)
  初始化 csr_plan 和 sfence_plan；任一 enable=0 时该计划为无目标
  rob_key = choose_rob_start_key()

  对 uid=0..N-1：
    csr_hit    = (csr_plan.next_uid    == uid)
    sfence_hit = (sfence_plan.next_uid == uid)

    若 csr_hit：
      生成 CSR_CONTROL 条目
      以 uid 重新预约 CSR；若新目标 >= N，CSR 计划变为无目标
      若 sfence_hit：以 uid 重新预约 SFence；若新目标 >= N，SFence 计划变为无目标
    否则若 sfence_hit：
      生成 SFENCE_CONTROL 条目
      以 uid 重新预约 SFence；若新目标 >= N，SFence 计划变为无目标
    否则：
      按现有随机/边界/地址复用逻辑生成普通访存条目

    将该条目写入当前 uid，并把 rob_key 前进一位

  分配 uid=N：
    生成 CHECK_STORE 条目，使用当前 rob_key
  init_status_for_main_table()
  check_main_table_complete()
```

固定优先级为 CSR 高于 SFence。二者同一 UID 命中时，该 UID 只生成 CSR，SFence 的旧目标视为已消费并从该 UID 重新预约，不能保留旧目标至下一轮。任何新目标 `>= N` 都直接放弃；不得生成到末尾 `check_store`、不得扩表、不得把目标截断到 `N-1`。

控制条目生成函数只填写 `uid`、`op_class`、连续 `robIdx` 和明确的无效 LSQ/普通访存字段；它不调用普通地址、`fuType`、`fuOpType`、边界 profile 或地址复用逻辑，也不加入 recent load/store pool。普通条目原有随机流程不变。

## 数据结构、状态和所有权

### 新增分类与主表/状态字段

在 `memblock_dispatch_types.sv` 中新增以下类型，枚举具体数值连续追加，不能修改已有编码：

| 类型 | 最小字段或枚举值 | 写者和用途 |
|---|---|---|
| `memblock_op_class_e` | `MEMBLOCK_OP_CLASS_CSR_CONTROL`、`MEMBLOCK_OP_CLASS_SFENCE_CONTROL`、`MEMBLOCK_OP_CLASS_CHECK_STORE` | 主表生成和 manual table；唯一的静态控制分类。 |
| `memblock_control_kind_e` | `NONE/CSR/SFENCE/CHECK_STORE` | 从 `op_class` 纯派生；供 status、barrier 和 token 使用。 |
| `memblock_control_state_e` | 下文定义的 CSR/SFence/`check_store` 状态 | 只由控制 admission、control service、已验证的 worker sendover helper 改写。 |
| `memblock_control_completion_profile_e` | `RUNTIME_CSR_SNAPSHOT`、`L2_FLUSH_LEVEL` | token 决定完成事实来源，禁止把 L2 flush 当作 CSR snapshot。 |
| `memblock_control_owner_t` | `valid, uid, dynamic_epoch, action_generation, kind` | 所有 token、完成记录、状态迁移使用同一 owner 比较函数。 |
| `memblock_csr_control_action_t` | owner、profile、expected runtime 字段、snapshot baseline、L2 phase | CSR queue 的持久工作项。 |
| `memblock_sfence_control_action_t` | owner、expected fence 字段、`start_item()` 前冻结的 C0 event baseline、`l2tlb_reset_epoch_at_arm`、`sfence_c0_match_armed` | SFence queue 的持久工作项；armed 或 sendover owner 都可接收同拍 C0，service 只在 sendover 后消费。 |
| L2 done wire observation | `memblock_sync_pkg` 定义的 primitive `valid/level/observation_seq/control_reset_epoch` latest 槽；DCache monitor 只发布它。`control_reset_epoch` 是控制生命周期自己的 reset 代际，不复用 L2TLB epoch；ASSERT/RELEASE baseline 与 high/low 匹配必须同代。 | seq 层 service 读取 wire observation 后转换为本地高层完成事实。 |
| `sbIsEmpty` wire observation | `memblock_sync_pkg` 定义的 primitive `valid/level/observation_seq/control_reset_epoch` latest 槽；ctrl monitor 为每个有效 sample 发布。它与 owner 分离，控制 service 通过 request 的 sendover baseline 归属。 | request sendover 前为高的旧 level 不得完成新 request。 |
| `dispatch_raw_ctrl_t.sb_is_empty_observation_seq` | common/sync 层 raw struct 的 primitive 字段；与 raw ctrl 一起冻结的 `sbIsEmpty` observation 序号。它是 raw 的不可变 provenance，不得在 deferred 消费时从 latest 槽重新读取。 | raw 在 sample 20 采集、sample 24 才消费时，仍使用 sample 20 的序号。 |
| C0/C4 wire lifecycle record | `memblock_sync_pkg` 以 `lifecycle_event_seq + l2tlb_reset_epoch` 为键保存的 primitive payload、anchor/due、claim code 和消费位；armed 控制 fence 的 C0 可在 sendover 前落记录，普通 fence 在 C4 后按既有逻辑回收。它不使用 `control_reset_epoch`。 | seq 层将匹配 record 转为控制 action 的 C0/C4 完成事实。 |
| `l2_flush_level_hold` | 唯一 CSR driver hold 状态，至少包含 `valid`、owner(uid/epoch/generation)、完整 CSR baseline 和 `release_requested`。driver 是该私有 hold 的唯一写者；control service 只能发布 reset request，不能直接清 hold。 | ASSERT 后即使 sequencer 暂无 item，driver idle 仍依据同一 baseline 保持 `flush_l2_enable=1`。 |
| `L2_FLUSH_ASSERT/RELEASE` metadata | 写入 `csr_ctrl_agent_agent_xaction` 的非 DUT 字段，携带 owner 和动作类型；sequence 只填写 metadata，driver 在真实 item 边界建立/清除 hold。 | sequence 不直接访问 driver 私有变量；ASSERT item 完成后由 driver 深拷贝 baseline。 |
| `control_post_reset_producer_ack` | 以 `control_reset_epoch + producer_kind` 保存 CSR/DCache/ctrl 三个 producer 的首个 post-reset sample 确认；每个 producer 只有一个有界槽。 | `complete_control_runtime_reset()` 只检查这三个 ack 与 driver ack，不读取任何控制完成 observation。 |
| `control_csr_runtime_baseline` | CSR monitor 在当前 control epoch 的 ready 后首次实际发布 runtime snapshot 时写入 `valid/epoch/first_snapshot_seq` 的有界槽。它只作为 action 可读取 global latest 的代际 gate；每个 action 从 latest API 一次读取当前 raw/seq 并冻结到自己的 token。 | `CSR_CONFIG_PENDING` 等该槽与当前 epoch 匹配后才创建 token。 |
| `l2_flush_release_request_q` | control service 写入、CSR worker 消费的有界 owner 化 RELEASE 请求队列；每项包含完整 owner、control reset epoch 和 release baseline。它是 high 完成到 RELEASE 驱动之间的持久交接，不依赖 worker 是否正好在某个检查点运行。 | `WAIT_L2_FLUSH_DONE` 只 push 一次；worker 按 owner pop 后生成 `L2_FLUSH_RELEASE` item，并回写 consumed/sendover 事实。 |
| control-runtime bootstrap | 任一 active control topology 的主表构建完成后建立首个 `control_reset_epoch` 和 reset request 的一次性初始化；它不等待物理 reset edge，也不创建控制 token。 | AUTO 或 manual-control 主表完成后共同 hook 调用 `initialize_control_runtime_bootstrap()`，随后由四个 producer ack 打开 runtime ready。 |

`control wire metadata` 与 `control owner` 分属不同编译层：CSR/Fence agent package 和 `memblock_sync_pkg` 不得引用只在 `seq_pkg` 中定义的 `memblock_control_owner_t` 或 seq enum。agent xaction、driver 和 common package 只使用固定宽度 primitive 字段（`metadata_valid`、`owner_uid`、`owner_dynamic_epoch`、`owner_action_generation`、`owner_kind_code`、`action_kind_code`、`control_reset_epoch`）；control service 在 seq 层通过唯一转换 helper 将 wire metadata 转为高层 owner/enum。这样保持 `agent -> common -> seq` 的现有编译顺序，不引入 package 反向依赖。

该边界必须落实到文件接口：agent xaction/driver 的字段类型只能是 `bit`、定宽 packed integer 或现有 agent/common 可见的 primitive typedef；不得在 agent package 的声明、constraint、driver helper、`memblock_sync_pkg` API 参数或 common package struct 中写 `memblock_control_owner_t`、`memblock_control_kind_e`、`memblock_control_completion_profile_e`、`memblock_l2_flush_done_observation_t`、`sfence_lifecycle_observation` 等 seq 层类型。`owner_kind_code`/`action_kind_code` 使用固定数值编码，并在 seq 层提供唯一 `decode_control_wire_metadata()` 转换；未知编码、metadata version 不匹配或 owner 字段不完整时立即 `uvm_fatal`。`memblock_sync_pkg` 还必须独立定义和保存 L2 done、`sbIsEmpty`、C0/C4 的 primitive wire observation/record，并只提供 publish/get/clear API；DCache/ctrl/Fence monitor 仅调用这些 API。seq 层 `memblock_control_barrier_service` 读取 wire record 后再转换成高层 owner/完成事实，`status_transaction` 只保存已匹配结果。新增 agent/common 文件仍按现有 filelist 的 agent/common 在前、seq 在后的顺序加入，不能通过反向 import 绕过编译依赖。

`status_transaction` 为控制条目新增控制元数据，至少包含：控制类型/状态、静态 barrier 标记、已绑定 owner、`control_commit_ready`、CSR runtime snapshot baseline/expected/已归档 snapshot 和 seq、CSR control reset generation、`flushSb` request id、SFence C0 lifecycle event id/due sample/`l2tlb_reset_epoch_at_arm`、L2 assert/release observation baseline、`control_reset_epoch`、high/low 是否已经消费、RELEASE request id/queued/consumed/sendover 标志。普通访存状态不复用这些字段，control 状态默认 `NONE`。

`common_data_transaction` 新增以下唯一来源：

| 对象 | 语义 |
|---|---|
| `active_control_barrier_uid` 和 kind | 当前唯一屏障 owner。静态等待时只保存 UID/kind；动作开始后 status 保存完整 owner。 |
| `csr_control_action_q`、`sfence_control_action_q` | token 的持久 FIFO；控制动作按屏障串行，队列仍保留用于 worker 启动先后和 event 不丢失。 |
| `csr_control_action_available_ev`、`sfence_control_action_available_ev` | `uvm_event`，只用于唤醒 worker。入队完成、状态更新完成后才 trigger。 |
| owner 化 `flushSb` 生命周期记录 | 全局只允许一个 active request 和一个有界 completed owner slot；按 `req_id` 保存 attached、sendover baseline 与 completed 副本，control service 以 request id 直接查询，消费确认后立即清 slot，reset/abort 也清理。 |
| SFence C0/C4 wire lifecycle record | adapter 通过 `memblock_sync_pkg` 发布/持久保存 primitive `lifecycle_event_seq + l2tlb_reset_epoch + sample` 事实；不携带 UID、不让 monitor 写 status。seq service 负责转换和 owner 匹配。 |
| L2 done wire observation | DCache monitor 向 `memblock_sync_pkg` 覆盖发布 primitive latest 槽；不建无限 raw queue。 |

现有 `memblock_flushsb_req_t` 必须增加可选 `owner_valid`、`owner_uid`、`owner_dynamic_epoch`、`owner_kind`、`action_generation`。周期性或既有 directed 请求保持 `owner_valid=0`；只有 owner 化请求才写入生命周期记录。为兼容现有 legacy producer，保留 `push_flushsb_request(source)` 的无 owner 入口，另增加 `push_owner_flushsb_request(owner, output req)`；后者负责分配 `req_id`、入队并把完整 request 返回给控制 service，避免调用方自行复制 request id。现有 `mark_flushsb_driven()` 拆成两个语义固定的 helper：

1. `mark_flushsb_request_attached_to_lsqcommit_xaction()` 在 `try_pop_flushsb_request()` 成功后、`start_item()` 前调用，只保存 active request/attached 事实；它不置 `flushsb_waiting_empty`，也不允许 `sbIsEmpty` 完成该请求。
2. `mark_flushsb_request_driver_sendover()` 在同一 xaction 的 `finish_item()` 返回后调用，才置 `flushsb_waiting_empty` 和 sendover 记录；`update_sb_is_empty()` 只完成已经 sendover 的 active request。

`io_mem_to_ooo_ctrl_agent_agent_monitor` 已仅在 `dispatch_flushsb_waiting_empty` 为真时把 `sbIsEmpty` 收入 raw ctrl；为防止 sendover 前已为高的 level 被错误消费，在 `memblock_sync_pkg` 新增 primitive `sbIsEmpty` wire latest 槽，包含 `valid/level/observation_seq/control_reset_epoch`，并在每个有效 ctrl sample 单调更新。每条进入 raw ctrl 的记录必须同时冻结 `sb_is_empty_observation_seq`（以及已有的 `cycle`/sample provenance）；即使 raw 在 deferred FIFO 中延迟，也只能使用自身冻结的序号。`mark_flushsb_request_driver_sendover()` 在 `finish_item()` 返回后的发送边界记录当前 observation 序号为该 request 的 baseline；`update_sb_is_empty(raw)` 只在 active request 已 sendover、raw 的 `sb_is_empty_observation_seq > sendover_baseline` 且 raw level 为 1 时完成 active request。`try_pop_flushsb_request()` 还必须把 attached 但尚未 sendover 的 active request 当作 busy，保证公共 consumer 在一个请求完成前不会取出第二笔。完成后 owner request 的副本按 `req_id` 保存到有界 completed map，控制 service 消费并确认 owner 后立即删除；无 owner 的 legacy request 保持现有直接清 active 的语义。reset release 的首个有效 ctrl sample 只发布 `post-reset producer ack`，不更新可消费 `sbIsEmpty` latest/raw；`control_runtime_ready` 打开后的下一 ctrl sample 才按当前 `control_reset_epoch` 发布 observation。这样不需要新建 `flushSb` monitor 或第二个 driver，也不会把上一拍已为高的 `sbIsEmpty` 错当作尚未实际驱动 pulse 的完成。

`apply_raw_ctrl_deq()` 必须把整条 immutable raw（至少 `sb_is_empty`、`sb_is_empty_observation_seq`、`cycle`）传给 `update_sb_is_empty(raw)`；禁止保留只接收 level 的接口作为控制完成入口。monitor 即使 `dispatch_monitor_capture_en=0`，也要更新 owner-neutral latest observation；若控制 topology 需要通过 raw/deferred 链路完成，则在控制动作启动前检查 capture gate 已打开，否则直接 `uvm_fatal`，避免请求永远等待。

### 状态机

所有控制标记先进入共同的 `WAIT_OLDER_ROB_COMMIT`。它们只在 `commit_cursor_uid == uid`、本轮 redirect-first/recovery 已完成、且不存在 active/pending redirect 时绑定 owner 并进入各自后续状态。

| 类型 | 状态顺序 |
|---|---|
| CSR | `WAIT_OLDER_ROB_COMMIT -> CSR_CONFIG_PENDING -> CSR_SENDOVER -> WAIT_CSR_RUNTIME_SNAPSHOT -> CONTROL_COMMIT_READY -> terminal_done` |
| SFence | `WAIT_OLDER_ROB_COMMIT -> WAIT_FLUSHSB_REQ -> WAIT_SB_EMPTY -> SFENCE_REQ -> SFENCE_SENDOVER -> WAIT_L2TLB_FLUSH_EFFECTIVE -> CONTROL_COMMIT_READY -> terminal_done` |
| `check_store` | `WAIT_OLDER_ROB_COMMIT -> CHECK_STORE_FLUSHSB_PENDING -> CHECK_STORE_WAIT_SB_EMPTY -> CHECK_STORE_L2_CSR_ASSERT -> CHECK_STORE_WAIT_L2_FLUSH_DONE -> CHECK_STORE_L2_CSR_RELEASE -> CHECK_STORE_WAIT_L2_FLUSH_IDLE -> CONTROL_COMMIT_READY -> terminal_done` |

`CONTROL_COMMIT_READY` 是控制条目专用 commit 条件。它不要求 `writeback`、`pass`、`required_targets_done()`、LQ/SQ deq 或 issue target；但仍要求 control-active ROB map、无 active redirect、owner/state 完整匹配。

## Admission、ROB commit 与屏障推进

### 控制 admission 分流

抽象功能描述：`admit_control_marker()` 在普通 admission 到达控制 UID 时，仅建立 control-active ROB 可见性和屏障，不创建 LSQ/issue 工作。它保持主表 admission 前缀连续，以便现有 commit cursor 能识别该 ROB head。

实现 flow：

```text
next_uid_needs_lsq_admission():
  先检查 global redirect/flush gate
  读取 next_uid 的 op_class
  若存在 active control barrier 且 next_uid 大于 barrier_uid：返回不可 admission
  若 next_uid 是控制标记：返回 control 分流，不调用 derive_op_behavior()
  否则沿用现有 LSQ/non-LSQ 普通路径

admit_control_marker(uid):
  校验 uid 是当前 next admission UID 且没有另一个 active barrier
  使用同一 robIdx 调用专用 activate_control_uid():
    写 uid_by_active_rob、status.active、status.enq 和 admission prefix
    不写 uid_by_lq/uid_by_sq、不分配 LSQ、不给 issue queue
    写 control 专用无 LSQ terminal 条件：issue_ready=1、lsq_deq=1
  status.control_state = WAIT_OLDER_ROB_COMMIT
  active_control_barrier_uid = uid
  不调用 complete_admission()，不调用 issue scheduler
```

`collect_lsq_candidates()` 遇到控制 UID 必须停止收集，不能把控制项或其后的普通 UID 塞入同一 LSQ enqueue batch。`admit_non_lsq_if_ready()` 也必须在调用 `lsq_ctrl_model::derive_op_behavior()` 前先判断控制 `op_class`，避免当前 unsupported `fuType` fatal。

控制条目即使为满足公共 admission 状态而写入 `issue_ready=1`，也绝不能进入普通 issue route。`issue_queue_scheduler::route_uid()` 的第一条分支必须读取主表 `op_class`，若为 CSR/SFence/`check_store` 立即返回；`route_all_ready_uids()` 仍可扫描有限 active window，但控制 UID 不生成 load/STA/STD issue item。`prepare_issue_route_for_uid()` 对控制 UID 也只允许写公共 admission 所需的可见状态，不得调用 `route_uid()`。这是 scheduler 侧的必要早退，不能只依赖 admission 分流。

`validate_main_table_entry()` 也必须有同一条 control-neutral 早退，且位置在现有 atomic/CBO 检查、`validate_main_transaction()` 和 `derive_op_behavior()` 之前。新增的 `validate_control_main_table_entry()` 只校验控制条目的静态结构：`op_class` 属于 CSR/SFence/`check_store`，UID/`robIdx` 在范围内，`numLsElem=0`，LQ/SQ 索引和 LSQ flow 为无效/中性值，普通地址、`fuType`、`fuOpType`、边界和地址复用字段不参与普通语义；校验通过后直接返回。控制条目不得先进入 `derive_op_behavior()` 再依赖该 helper 对中性字段“容忍”，否则任一新控制字段都可能被普通 behavior 校验误判。

现有 `complete_admission()` 会立即调用 `issue_sched.prepare_issue_route_for_uid()`；控制分流不得复用它。`activate_control_uid()` 只能复用 `activate_uid(uid, 0, 0)`、`mark_uid_enqueued(uid)` 和 active ROB map 的公共语义，再写 control 专用 `enq=1`、`issue_ready=1`、`lsq_deq=1` 可见状态；它不能调用普通 issue route、LSQ reservation、`derive_op_behavior()` 或普通 `complete_admission()`。这样既能复用唯一 ROB map/顺序 admission 真源，也不需要改造 `lsq_ctrl_model` 为控制指令分配器。

### redirect 前静态屏障的恢复

抽象功能描述：`preserve_static_control_marker_on_redirect()` 让比控制标记更老的普通访存按既有 redirect/reissue 重新执行，同时保留控制标记的静态屏障身份，不创建第二个动作实例。

`apply_redirect_flush_range()` 扫描 active window 时按以下分流：

```text
若命中的 uid 不是控制标记：沿用 prepare_uid_for_redirect_reissue()

若命中的 uid 是 WAIT_OLDER_ROB_COMMIT：
  不调用 prepare_uid_for_redirect_reissue()
  不递增 control uid 的 dynamic_epoch
  不删除其 active ROB map，不取消 barrier，不删除任何不存在的 token/request
  不把该 control uid 计入 oldest_flushed_uid；rollback_max_enqueued_uid() 只回退到本次真正被 flush 的最老普通访存 uid

若命中的 uid 是已开始动作的控制标记：
  uvm_fatal
  原因：此前 UID 已形成不可回滚 terminal_done 前缀，年轻 UID 又被屏障阻止；
  覆盖该 control robIdx 的普通 redirect 违反本专项顺序不变量。
```

回退后，普通 UID 重发到静态控制 UID 时不能把已 active 的控制项当作重复 admission。新增一个只恢复 admission prefix 的 helper：它验证该 UID 仍是相同的静态 control-active 标记，然后只把 `max_enqueued_uid` 重新推进到该 UID；不重新 `activate`、不重写状态、不入队 action。这个 helper 必须由 `next_uid_needs_lsq_admission()` 的 control 分流先调用，不能依赖 `collect_lsq_candidates()` 发现该 UID；否则普通候选收集会先调用 `derive_op_behavior()`。这样 `uid=8` redirect 到 `uid=5` 时，UID `5..9` 可按原逻辑重发，随后重接 UID `10` 静态屏障，直到其前缀真正完成。`apply_redirect_flush_range()` 需要分别维护“真正被 flush 的普通 UID”与“被保留的静态控制 UID”；前者用于 `oldest_flushed_uid`，后者只保持 active/barrier，不参与 `rollback_max_enqueued_uid()`。

动作已开始后，未覆盖 control ROB 的迟到/stale redirect 仍交给既有合法性判断；只有真正覆盖当前 control ROB 的 redirect 必须 fatal。global reset/testcase abort 是独立清理路径，不赋予普通 redirect 取消并重发控制动作的语义。

### 控制 commit 与 retire 分流

抽象功能描述：`select_control_head_candidate()` 只在控制 UID 已处于 `CONTROL_COMMIT_READY` 且正是 `commit_cursor_uid` 的 modeled ROB head 时返回该 UID；它不与普通访存 commit batch 混合。

实现 flow：

```text
build_lsqcommit_xaction():
  先沿用现有 normal/fault candidate 选择
  若当前 head 是控制标记：
    不调用 derive_op_behavior()，不设置普通 pending store/MMIO 或 scommit 语义
    若 control_state == CONTROL_COMMIT_READY，则选择单个 control head
    否则只发送保持当前 pendingPtr 的 idle xaction

send_lsqcommit_cycle():
  先把 xaction 交给已有 LSQ commit driver
  normal/fault/control 三类最多选择其中一类
  control head 送出本拍 xaction 后调用 mark_control_rob_commit_uid():
    校验 cursor、ROB key、control-active、无 redirect、commit-ready
    写 status.rob_commit=1 和无 LSQ deq 依赖
    调用 try_retire_control_committed_uid()

try_retire_control_committed_uid():
  仅接受 control op_class + CONTROL_COMMIT_READY + rob_commit
  写 success=1、terminal_done=1，调用现有 retire_active_uid()
  不检查普通访存 writeback/pass/target done
```

`lsq_commit_handler` 的 normal batch selector、fault selector、`fault_uid_is_store_exception()`、`build_lsqcommit_xaction()` 的 head behavior 推导都必须先识别 control op_class 并绕开普通 `derive_op_behavior()`。`mark_control_rob_commit_uid()` 需仿照 normal batch 更新 `committed_rob_watermark`、`commit_cursor_uid=uid+1` 并调用 `rebase_framework_head_from_commit_cursor()`；不能只写 `status.rob_commit`，否则 cursor 会永久停在已终态 control UID。`try_retire_control_committed_uid()` 不得复用 `try_retire_committed_uid()` 的普通 `pass/required_targets_done()` 条件。控制 commit 成功后仍由现有 cursor/rebase 逻辑前进；只有观察到该 UID `terminal_done` 后，control service 才清除 barrier，下一 service/admission 边界才允许年轻 UID 继续进入。

## 控制 service 的调用顺序

`memblock_control_barrier_service` 是独立 helper，持有 `common_data_transaction` 与 `lsq_commit_handler` 的引用。topology 判定只读取由公共 plus 冻结的 mode snapshot，不通过 testcase 类名、派生类名、`VSEQ_MAIN`、`MEMBLOCK_USE_MANUAL_MAIN_TABLE` 或其他运行期现象猜测：`uses_control_barrier_topology()` 对 `AUTO_MAIN_TABLE` 和 `MANUAL_CONTROL_MAIN_TABLE` 返回 1；`uses_auto_control_barrier_topology()` 只用于自动 `N+1` 建表、CSR/SFence 随机预约和末尾 `check_store`。`MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE` 是唯一配置输入，`memblock_sync_pkg` 中的 snapshot 是唯一运行期读取对象；`reset_all_tables()` 不得清除或改写该 snapshot。

1. 在 `memblock_sync_pkg` 定义 `memblock_control_worker_topology_mode_e={DISABLED=0,AUTO_MAIN_TABLE=1,MANUAL_MAIN_TABLE=2,MANUAL_CONTROL_MAIN_TABLE=3}`，并由 `seq_csr_common::get_control_worker_topology_mode()` 返回已经过范围校验的值。新增 `initialize_control_worker_topology_from_plus()`：它将 plus 值冻结到 `memblock_sync_pkg`；同一测试中再次调用只能读到相同值，否则 `uvm_fatal`。`tc_base` 在 `seq_csr_common::reload_from_plus()` 后调用它；`basicTest` 则必须先解析 `VSEQ_MAIN`、得到场景是否具备 dispatch capability，再调用它和后续 compatibility check，绝不能让 VSEQ 写 mode。与此同时提供只读纯判定 `uses_auto_control_barrier_topology()` 和 `uses_control_barrier_topology()`，以及派生位 `control_worker_topology_active=uses_control_barrier_topology()`。这些 helper 只检查 snapshot，不读取或校验 `MEMBLOCK_USE_MANUAL_MAIN_TABLE`。
2. 控制 worker 不安装到 agent phase `default_sequence`。`basicTest` 解析 `VSEQ_MAIN` 后只允许两个具备 dispatch 能力的专项 VSEQ 在 active mode 启动 worker：`memblock_dispatch_real_smoke_vseq` 对应 AUTO，`memblock_dispatch_manual_control_vseq` 对应 MANUAL_CONTROL。二者在 `body()` 中通过 `p_sequencer.csr_ctrl_sqr` 和 `p_sequencer.fence_sqr` 显式并行启动 `memblock_csr_control_base_sequence` 与 `memblock_sfence_control_base_sequence`，并与主 dispatch/responder 生命周期一起收敛。mode 为 DISABLED 或 MANUAL_MAIN 时不启动控制 worker；任何 legacy testcase、software-only/no-dispatch 场景或非允许 VSEQ 传 mode=`1/3` 均在入口 `uvm_fatal`。这样每个 active sequencer 只有一个 producer，且 VSEQ 是 worker 的唯一生命周期 owner。
3. testcase 和 VSEQ 不新增 `get_control_worker_topology_mode()`、allowlist 或任何 mode 写入逻辑。它们仍负责选择自身的场景与 main sequence；main sequence 在 body 入口只校验 plus snapshot 是否与自己的 builder 能力匹配：`memblock_main_dispatch_auto_build_main_table_base_sequence` 接受 DISABLED 或 AUTO；DISABLED 保持既有 generic `build_main_table()` 行为，AUTO 要求 `MEMBLOCK_USE_MANUAL_MAIN_TABLE=0` 并进入 `build_control_auto_main_table(N)`；普通 direct-manual/cancel-reconcile sequence 只接受 MANUAL_MAIN；新增 direct manual-control sequence 只接受 MANUAL_CONTROL。任一 mismatch 均 `uvm_fatal`，不能根据 VSEQ/testcase 名称重写 plus，也不能切换到别的 builder。
4. 现有 `tc_dispatch_real_mixed_wb_smoke`、`tc_dispatch_real_mixed_sta_wb_smoke` 和 cancel-reconcile 的 preset cfg 显式提供 mode=`2`，从而在其 direct manual builder 启动前完成匹配；它们不再覆写 mode getter。`basicTest + memblock_dispatch_real_smoke_vseq` 在 mode=`0` 时保持 legacy generic main-table 行为，在专项 AUTO cfg 中设为 mode=`1` 后才生成控制主表；`tc_dispatch_real_smoke` 本身不支持 mode=`1`，请求时 fail-fast。手工控制只使用 `basicTest + memblock_dispatch_manual_control_vseq`，并由对应 preset/命令行设 mode=`3`；VSEQ 本身不登记或设置该值。
5. software-only 与无 dispatch topology testcase 若传 mode=`1` 或 mode=`3`，在 build/config 完成、启动 worker 前立即 `uvm_fatal`；mode=`2` 也只能由能够启动普通 direct-manual main sequence 的场景使用。此项是 plus 配置与场景选择的必要一致性校验，不是从场景反推或覆盖 mode。`MEMBLOCK_USE_MANUAL_MAIN_TABLE` 仍仅保留在 DISABLED generic builder 的 legacy random/import 分流中。

只有 `control_worker_topology_active=1` 的路径可启动新增 worker、control barrier service、bootstrap 和 worker shutdown。**无论 topology mode 为何，既有 `service_real_dispatch_flow()` 都必须照常运行**；manual 主表 sequence 继承 auto base 时尤其不能因为 mode 不是 AUTO 就跳过原有 dispatch/monitor/issue/commit service。bootstrap 的唯一真实调用点是任一控制主表的共同 post-build hook：在 AUTO 的 `build_main_table()` 或 MANUAL_CONTROL 的 directed main-table builder **返回后**、`service_real_dispatch_flow()` 前调用一次 `initialize_control_runtime_bootstrap()`，建立首个 `control_reset_epoch=1`、清空旧 observation 并发布 `control_reset_request`；不能放在建表前，因为建表会调用 `reset_all_tables()` 清 testcase runtime 状态。control runtime handshake 的 epoch/request/ready、CSR driver ack 和三条 producer ack 固定存放在 `memblock_sync_pkg`，或由 `reset_all_tables()` 明确豁免；建表只能清主表、status、ROB/LSQ runtime，不能清这些 bootstrap 字段。MANUAL_MAIN_TABLE 和 software-only 路径不调用该 helper，也不执行 control bootstrap-ready 检查。现有 `service_real_dispatch_flow()` 在 `rst_n!=1` 或 `reset_backend_done!=1` 时会在调用 `service_monitor_once()` 前直接 `continue`，因此新增 control reset gate 不能只放在后者内部；任一 active control topology 的外层 reset 分支必须先调用 `control_barrier_service.begin_control_runtime_reset(PHYSICAL_RESET)`，再 `continue`。正常 service 中的 active-control topology reset early-return 也调用同一 helper，以覆盖 L2TLB reset 等不经过外层物理 reset判断的边界。调用顺序固定为：

```text
generic main sequence body：
-> 读取唯一 plus mode snapshot；若为 DISABLED，执行既有 build_main_table()，随后校验表中不存在 control op_class，不创建 control barrier service、不调用 bootstrap
-> 若为 AUTO，创建并绑定新增 control barrier service；校验 MEMBLOCK_USE_MANUAL_MAIN_TABLE=0，再调用 build_control_auto_main_table(N)
-> 若为 MANUAL_MAIN 或 MANUAL_CONTROL，直接 uvm_fatal，防止把 direct-manual flow 送入 generic body
-> 仅 AUTO 在共同 post-build hook 调 initialize_control_runtime_bootstrap()（仅一次建立 epoch/request，不创建 token）
-> 所有允许的 mode 都继续 service_real_dispatch_flow()

MANUAL_CONTROL main sequence body：
-> 读取 plus mode snapshot；若不是 MANUAL_CONTROL_MAIN_TABLE 直接 uvm_fatal
-> 创建并绑定同一 control barrier service
-> 调专用 direct manual-control builder；该 builder 完成 import/校验但不读 MEMBLOCK_USE_MANUAL_MAIN_TABLE
-> 调相同 post-build hook 初始化 bootstrap
-> service_real_dispatch_flow()

MANUAL_MAIN main sequence body：
-> 读取 plus mode snapshot；若不是 MANUAL_MAIN_TABLE 直接 uvm_fatal
-> 调既有 direct manual builder/import，不创建 control barrier service、不调用 bootstrap
-> service_real_dispatch_flow()

service_real_dispatch_flow 每拍：
-> 若 topology 为 active control：检查 control-runtime bootstrap 已完成；未完成直接 uvm_fatal，不得在 service loop 内初始化
-> 若 rst_n!=1 或 reset_backend_done!=1：active control topology 调 begin_control_runtime_reset(PHYSICAL_RESET)，随后所有 topology 均按既有 flow continue
-> tick_dispatch_service_cycle()
-> collect_runtime_context_events()
-> monitor_adapter.service_l2tlb_sfence_events()
-> 若 reset_backend_done!=1 或 l2tlb_reset_active：active control topology 调 control_barrier_service.begin_control_runtime_reset()；随后按既有 service return
-> collect_monitor_event_batch()
-> exception_redirect_replay_task()
-> drain/reconcile 本轮 LSQ sideband
-> active control topology 调 control_barrier_service.service_once()
-> 本轮结束后现有 route_all_issue_queues()
```

reset release 后采用明确的两阶段协议，不能让 ready 与 observation publish 相互等待：CSR、DCache、ctrl monitor 在观察到当前 `control_reset_epoch` 对应的 `control_reset_request` 后，各自在首个有效 post-reset sample **无条件**写入 `post-reset producer ack(control_reset_epoch, sample_seq)`；该 ack 不更新 runtime snapshot、`sbIsEmpty` latest/raw 或 L2 done latest，因而不能完成任何控制动作。CSR monitor 在该 ack 点同时清自己的 `has_last_runtime_csr`/producer-local baseline，但不清全局 snapshot 序号；这样 ready 后的首个实际 CSR sample 必然按“新 producer baseline”发布。CSR driver 也必须消费同一 request 并发布 `driver_reset_ack(control_reset_epoch)`。`service_monitor_once()` 在 driver ack 与三个 producer ack 齐备、control topology ready 后调用 `complete_control_runtime_reset()`，置 `control_runtime_ready=1`。从各 producer 的**下一**有效 sample 起，才按当前 `control_reset_epoch` 发布可消费 CSR snapshot、`sbIsEmpty` 和 L2 done observation；CSR monitor 在首次 runtime snapshot 发布时写 `control_csr_runtime_baseline(valid, epoch, first_snapshot_seq)`。该槽仅证明 current epoch 已有有效 global latest；后续每个 action 均通过 existing latest API 单次读取当前 raw/seq 并冻结到 token，不能长期复用首次 payload。C0/C4 只消费 ready 后建立、且与 token 的 `l2tlb_reset_epoch_at_arm` 相同的 lifecycle record。在 `control_runtime_ready=0` 期间，`service_once()` 不创建 token/request，也不消费 CSR snapshot、`sbIsEmpty`、C0/C4 或 L2 done completion。正常路径中，该位置保证 C0/C4、CSR raw、`sbIsEmpty` deferred raw 与 redirect-first 仲裁已先完成。service 每次只查询 `active_control_barrier_uid` 对应的单条 status、精确 request/event id 和 latest done observation；不遍历历史主表、全部状态表或无界 raw queue。

物理 reset 分支的文字伪代码如下，确保外层 `continue` 不绕过 reset 清理：

```text
service_real_dispatch_flow 每拍醒来后：
  若 rst_n != 1 或 reset_backend_done != 1：
    若 topology 为 active control：调用 begin_control_runtime_reset(PHYSICAL_RESET)
      该 helper 只在本 epoch 尚未标记 reset-active 时递增 epoch、清理动作队列/完成事实并发布 driver reset request；重复拍只返回，不重复清理
    跳过 monitor/service/issue/stop 检查，进入下一拍
  否则：
    执行 service_monitor_once() 的正常 flow
```

抽象功能描述：`service_once()` 在当前屏障的状态允许时创建一次 token/request，或消费一次精确完成事实推进状态。它不驱动 CSR/Fence interface，也不直接改 DCache/L2TLB 功能状态。

共同前置 flow：

```text
若没有 active barrier：返回
读取 barrier uid/status，校验 control-active ROB map 与 kind 一致
若 status.terminal_done：清除 barrier；返回
若 state == WAIT_OLDER_ROB_COMMIT：
  仅在 commit_cursor_uid == uid 且无 active/pending redirect 时：
    绑定 uid + 当前 dynamic_epoch + 新 action_generation
    按 kind 进入 CSR/SFence/check_store 的首个动作状态
否则按下列分支处理
```

重复 service tick、重复 level、旧 epoch token、错误 owner、过期 reset epoch 或状态已经离开消费者状态时必须不推进；若“当前 owner 却收到不可能的未来/重复完成事实”，使用 `uvm_fatal` 输出 uid、epoch、generation、状态、event/request id 和 sample/observation 序号。

## CSR 控制 Flow

### action queue 与 worker

`enqueue_csr_control_action(owner)` 的职责是持久化 CSR 工作项，不负责具体配置：

```text
校验 status 是 CSR_CONFIG_PENDING、owner 匹配且尚未 enqueue
构造 completion_profile=RUNTIME_CSR_SNAPSHOT 的 token
将 token push 到 csr_control_action_q
写 status 的 token/enqueue 记录
最后 trigger csr_control_action_available_ev
```

`memblock_csr_control_base_sequence` 是 `csr_ctrl_sqr` 的唯一 producer。它采用“先查 queue、为空才等待 event、醒来后重新查 queue”的循环；因此 worker 晚启动、多个 token 连续到达、event 先到都不会丢 CSR 动作。

```text
worker 主循环：
  若 csr_control_action_q 非空：pop 一个 token
  否则若 control worker 可退出：结束
  否则等待 csr_control_action_available_ev 或统一 shutdown 通知，随后重查

  若 token.profile == RUNTIME_CSR_SNAPSHOT：
    configure_csr_control_xaction(token, tr)
    drive_csr_control_xaction(token, tr)
  若 token.profile == L2_FLUSH_LEVEL：
    执行下文 check_store 保持型 CSR flow
  其他 profile：uvm_fatal
```

`CSR_CONFIG_PENDING` 的 service 分支先检查 `control_csr_runtime_baseline.valid=1`、其 epoch 等于当前 `control_reset_epoch`；随后调用现有 `get_latest_runtime_csr_snapshot(raw, seq)` 一次并要求 `seq >= first_snapshot_seq`，将该次返回的**当前** `raw + seq` 深拷贝到 token/status。条件未齐只保持当前状态，不能用 reset 前 latest、worker 本地缓存或发送过的 xaction 代替。该 action-local copy 不依赖 snapshot history；后续 CSR 标记或 `check_store` 可以各自重新冻结更新后的 latest 配置，不能复用启动 epoch 的第一份 payload。满足后才 enqueue token。

`configure_csr_control_xaction()` 首版固定使用 `CSR_CONTROL_PROFILE_V1`。它必须创建一个**不 randomize** 的 `csr_ctrl_agent_agent_xaction`，先以确定的零值/既有 driver-safe 默认字段初始化完整 xaction，再逐字段映射 token 中已冻结的 `dispatch_raw_csr_t` monitor baseline；`pre_pkt_gap=0`、`post_pkt_gap=0`、`flush_l2_enable=0` 和所有控制 metadata 也必须显式写定。不得把 `dispatch_raw_csr_t` 当作“完整 xaction”直接 copy，因为当前 raw snapshot 不包含所有 CSR driver 字段。随后只做一个可证明的 SATP 变化：保留当前合法 `satp_mode` 和 `satp_ppn`；若 baseline `satp_asid == 16'hffff` 则写 `16'h0000`，否则写 `satp_asid + 16'h0001`；只置本 action 所有的 `satp_changed=1`，并强制清零 `vsatp_changed`、`hgatp_changed`、`priv_virt_changed` 以及所有一次性 CSR write/trigger valid。不得把采样到的旧 change/write pulse 重驱给 DUT。若该结果与 baseline 仍无变化，fallback 为 `satp_asid ^= 16'h0001`；若 `raw_csr_payload_changed(baseline, expected)` 仍为假，直接 `uvm_fatal`。token 保存 expected runtime 字段和 baseline，monitor 必须能依据这些字段发布新 snapshot；不得把主表记录成 CSR payload，也不得选择值不变、monitor 不会发布新 snapshot 的 no-op 配置。后续 CSR 专项扩展只能进入该函数，不改变 `RUNTIME_CSR_SNAPSHOT` 完成协议。

`drive_csr_control_xaction()` 在 `start_item` 前记录 `runtime_csr_snapshot_seq_before_drive`，完成 `finish_item` 后仅调用受 owner 校验的 `mark_csr_control_sendover()`。`CSR_SENDOVER` 只代表 sequence 已完成接口交付，不代表 runtime snapshot 已确认。

### runtime snapshot 完成

`WAIT_CSR_RUNTIME_SNAPSHOT` 只接受以下全部条件成立的 monitor 事实：

```text
runtime_csr_snapshot_valid == 1
runtime_csr_snapshot_seq > status.runtime_csr_snapshot_seq_before_drive
monitor snapshot 与 token 的 expected runtime 字段匹配
当前 control reset generation 未变化（CSR snapshot 本身不携带 reset_epoch，不把不存在的字段作为完成条件）
```

满足时，control service 把 **monitor 已观察到的** `runtime_csr_snapshot` 及其 seq 克隆到 status，置 `CONTROL_COMMIT_READY`。已发送的 `csr_ctrl_agent_agent_xaction` 只作为临时 expected 配置，不能作为归档 snapshot 或完成证据。

`io_ooo_to_mem_csrCtrl_flush_l2_enable` 不属于此 profile：它不在 runtime snapshot changed 语义中，不能用 runtime snapshot 证明保持或完成；只能由后文 `L2_FLUSH_LEVEL` 处理。

## SFence 控制 Flow

### `flushSb` 阶段

`enqueue_control_flushsb_request(owner)` 的职责是把 SFence 或 `check_store` 的带 owner 请求放入现有公共队列，并按 `owner.kind` 保持各自的状态名：

```text
若 owner.kind == SFENCE：校验 state == WAIT_FLUSHSB_REQ
若 owner.kind == CHECK_STORE：校验 state == CHECK_STORE_FLUSHSB_PENDING
其他 kind：uvm_fatal
两类共同校验尚未分配 request id
构造 owner_valid=1 的 memblock_flushsb_req_t
调用 `push_owner_flushsb_request(owner, req)`，由公共 helper 分配 req_id、入 `flushsb_req_q` 并把完整 request 回存到 status
不触发新的 flushSb event
```

既有 `memblock_lsqcommit_dispatch_base_sequence::send_lsqcommit_cycle()` 继续轮询唯一 consumer `try_pop_flushsb_request()`。它成功取出后将 `io_ooo_to_mem_flushSb=1`，在 `start_item()` 前调用 `mark_flushsb_request_attached_to_lsqcommit_xaction()`；本拍 `finish_item()` 返回后再调用 `mark_flushsb_request_driver_sendover()`。control service 必须读取 sendover 记录而不是 active request/attached 记录，且按 owner kind 推进：SFence 的 `WAIT_FLUSHSB_REQ -> WAIT_SB_EMPTY`，`check_store` 的 `CHECK_STORE_FLUSHSB_PENDING -> CHECK_STORE_WAIT_SB_EMPTY`。随后按同一 owner/request id completed record 推进：SFence 的 `WAIT_SB_EMPTY -> SFENCE_REQ`，`check_store` 的 `CHECK_STORE_WAIT_SB_EMPTY -> CHECK_STORE_L2_CSR_ASSERT`。`finish_item()` 之前到达的 raw ctrl 仍按已有 deferred 链路保留，但不能被 `update_sb_is_empty()` 消费为该 attached 请求的完成。

`io_mem_to_ooo_ctrl_agent_agent_monitor` 在每个有效 ctrl sample 发布 owner-neutral `sbIsEmpty` latest observation；既有 `raw_ctrl_q -> deferred ctrl -> lsq_commit_handler::apply_raw_ctrl_deq() -> update_sb_is_empty()` 链路继续负责完成消费。`update_sb_is_empty()` 只在当前 active request 已 sendover、observation seq 新于该 request 的 sendover baseline 且 level 为 1 时完成 owner 化请求，在按 req_id 的 completed 记录中保留原 request 副本，再清现有 active request。control service 只有在 owner、request id 和 completed 记录都匹配时才可推进，避免 periodic 或其他 directed `flushSb` 的 `sbIsEmpty` 误解除本 SFence。

### SFence action、C0 与 C4

匹配 `sbIsEmpty` 后，`enqueue_sfence_control_action(owner)` 按“queue 先、状态后、event 最后”执行：写 `sfence_control_action_q`，将 status 改为 `SFENCE_REQ`，最后 trigger `sfence_control_action_available_ev`。

`memblock_sfence_control_base_sequence` 是 `fence_sqr` 的唯一 producer，使用与 CSR 相同的队列/event 取件规则。`configure_sfence_control_xaction()` 只生成 xaction 和 expected fence payload；`drive_sfence_control_xaction()` 只完成接口交付并将状态改为 `SFENCE_SENDOVER`。

`configure_sfence_control_xaction()` 首版固定生成 canonical 基础 SFence payload：`io_ooo_to_mem_sfence_valid=1`、`pre_pkt_gap=0`、`post_pkt_gap=0`、`rs1=0`、`rs2=0`、`addr=0`、`id=0`、`hv=0`、`hg=0`、`flushPipe=0`。该函数同时把上述字段复制到 token 的 expected fence 字段；不允许依赖现有 xaction 的随机默认值（尤其不能让 `valid=0` 导致 Fence monitor 不创建 C0），也不在配置函数内等待、驱动或更新完成状态。后续 SFence/HFENCE 具体 payload 扩展只能进入该函数，C0/C4 owner 匹配协议保持不变。

`drive_sfence_control_xaction()` 的抽象时序约束是：在 `start_item()` 之前从同步包读取并冻结 `last_allocated_l2tlb_event_seq` 与当前 `l2tlb_current_reset_epoch`，写入 token/status 的 `l2tlb_reset_epoch_at_arm`，并先置 `sfence_c0_match_armed=1` 作为 C0 pre-drive 匹配资格；随后完成 `start_item/finish_item`，只记录 sendover。此处不得读取或写入 `control_reset_epoch` 作为 C0/C4 的匹配 epoch。adapter 在 C0 到达时接受 armed 或 sendover owner，先保存 record；control service 只在 sendover 后消费该 record。不能在 `finish_item()` 后才建立 baseline 或 armed，因为 fence driver 和 monitor 可能在同一 posedge 交付并观察 C0。

SFence topology 检查必须拆成三个时点，避免把尚未注册的运行期对象误判为建表错误：

1. **建表/topology 初始化前**：只检查由 `MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE` 冻结的静态 control topology mode、静态 dispatch topology和 `dispatch_l2tlb_lookup_active`；此时不读取 `MEMBLOCK_USE_MANUAL_MAIN_TABLE`，也不读取尚未由 dispatch sequence `pre_body()` 注册的 `l2tlb_adapter_service_active` 或 owner。
2. **`pre_body()` 完成后的 service-ready/main-table-ready 边界**：确认 `l2tlb_adapter_service_active` 已置位、注册 owner 等于当前 main dispatch service，并确认 capture gate/monitor adapter 已准备；缺失或 owner 不匹配立即 `uvm_fatal`，再允许 SFence action 进入可运行状态。
3. **SFence action 真正启动前**：重新检查异步建立的唯一 L2TLB lifecycle/responder owner、当前 adapter owner 和 `l2tlb_current_reset_epoch`；owner/topology 缺失或不匹配直接 `uvm_fatal`。若 owner 已正确建立但 `l2tlb_post_reset_baseline_done(l2tlb_current_reset_epoch)=0`，`SFENCE_REQ` 保持等待、不得 arm/drive，直到既有 responder 发布 baseline proof；这属于正常异步初始化，沿用控制状态超时诊断，不得误报 topology fatal。现有 `fence_agent_agent_monitor` 在 no-dispatch topology 下会丢弃 raw fence，因此不能等到 C0/C4 超时后才诊断。

接口交付后，Fence monitor 仍按现有方式在观察到 `io_ooo_to_mem_sfence_valid` 时生成 raw SFence 和 lifecycle event。为了把 C0/C4 精确归属给控制 owner，补充两个 owner-neutral adapter 事实：

1. `schedule_sfence_invalidate()` 成功接收并登记 raw fence 后，先判断是否存在 `sfence_c0_match_armed=1` 或已 `SFENCE_SENDOVER` 的可匹配 owner（按 payload、`start_item()` 前冻结的 pre-drive event baseline 和 `l2tlb_reset_epoch_at_arm`）；只有可匹配的控制 fence 才在有界 map 中保存 `sfence_c0_observation`。C0 record 保留到对应 sendover 后由 service 消费，或在 L2TLB reset/abort 时清除。无匹配的 generic/manual fence 仍沿用既有 C4 work，但不占用 control lifecycle map。
2. `apply_due_sfence_invalidate()` 实际消费该 event、执行既有 C4 filter 删除/取消后，发布并保存同一 event id 的 `sfence_effective_observation`，包含 due sample 和 `l2tlb_reset_epoch`；即使删除条目数为 0，也必须发布完成事实。已被 control token claim 的记录在 service 确认 owner 后删除；未被 claim 的 generic 记录在 C4 更新后立即回收。L2TLB epoch 变化时清理整张有界 observation map。

control service 在 `SFENCE_REQ` 已确认现有 L2TLB post-reset baseline proof 后才 enqueue action。`SFENCE_SENDOVER` 只接受 pre-drive baseline 之后、payload 匹配、`l2tlb_reset_epoch` 等于 token 的 `l2tlb_reset_epoch_at_arm`、且 event id 新于该 baseline 的 C0 observation，记录 event id 后进入 `WAIT_L2TLB_FLUSH_EFFECTIVE`。`drive_sfence_control_xaction()` 必须在 `start_item()` 之前读取并冻结 `last_allocated_l2tlb_event_seq` 与当前 `l2tlb_current_reset_epoch` 到 token/status，并先置 `sfence_c0_match_armed=1`；`finish_item()` 返回后只写 SFence sendover，不得重新采样或覆盖 baseline/armed。adapter 可在 sendover 前用 armed owner 接住同拍 C0，但 service 只有在 sendover 后才消费该 record。这样 driver 与 monitor 同一 posedge 产生 C0 时，C0 event 仍满足 `event_seq > pre-drive baseline`。它只接受同一 event id、同一 `l2tlb_reset_epoch` 的 C4 effective observation 才置 `CONTROL_COMMIT_READY`；不能只读取“当前 pending queue 为空”或 latest level。拓扑检查按上文三个时点执行：建表前只做静态检查；`pre_body()`/service-ready 后检查 adapter 注册和 owner；动作启动前检查动态 lifecycle/responder owner 与 post-reset baseline proof。owner/topology 不一致必须 `uvm_fatal`，baseline 尚未到达则保持 `SFENCE_REQ`，因为现有 `fence_agent_agent_monitor` 在 no-dispatch topology 下会丢弃 raw fence。

不得以 `finish_item` 返回、`sfence_invalidate_pending_q` 为空、固定等两拍、或本地时间猜测替代 C4 completion。V2 的 `MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES=4` 仍由既有 adapter 使用；控制 service消费的是 adapter 已完成的准确 event。

## `check_store`：`flushSb` 后的 L2Cache Flush Flow

`check_store` 复用相同的静态屏障、owner 化 `flushSb` 和 CSR worker，但不产生 SFence/L2TLB flush。其 `CHECK_STORE_FLUSHSB_PENDING` 与 `CHECK_STORE_WAIT_SB_EMPTY` 分别复用 SFence 的请求登记和 owner 化完成匹配；匹配 `sbIsEmpty` 后直接进入 `CHECK_STORE_L2_CSR_ASSERT`。

### DCache done observation

在 `dcache_agent_agent_monitor` 的有效 post-reset sample 处新增发布：每拍把 `io_l2_flush_done` 写入 `memblock_sync_pkg` 的 latest observation，并让 `observation_seq` 单调递增，即使 level 未变化也递增。该对象只包含 `valid/level/observation_seq/control_reset_epoch`，不保存 UID、不建无界 queue、不写 control status。reset release 后首个有效 sample 先无条件发布 `post-reset producer ack(control_reset_epoch, sample_seq)`，但不发布可消费 L2 done；`control_runtime_ready` 打开后的下一 sample 才写入 latest observation。新增唯一 reset helper：`begin_control_runtime_reset()` 在主 service 首次检测到 reset/epoch 变化时递增 `control_reset_epoch`，清 control status、action queue、lifecycle observation 和 done/sbIsEmpty valid，并只发布 `control_reset_request`；它不能直接写 CSR driver 的私有 hold。CSR driver 在自身 reset 边界或观察到该 request 后，按 owner 清理 `l2_flush_level_hold` 并发布 `control_reset_ack`；`complete_control_runtime_reset()` 只有在三个 producer ack、control topology ready 且 driver reset ack 到达后才置 ready 标志。不得复用 L2TLB 的 `reset_epoch`。CSR token 在 drive 前保存同一 control reset generation，L2 done high/low 必须严格匹配该 epoch。

### `L2_FLUSH_LEVEL` 持续驱动

抽象功能描述：`drive_l2_flush_level()` 由 CSR worker 独占 `csr_ctrl_sqr`，只交付一次带 owner 的 ASSERT；CSR driver 随后以私有 `l2_flush_level_hold` 在 idle sample 保持 `flush_l2_enable=1`。control service 消费当前 generation 的 done-high 后，worker 再交付一次 low RELEASE，并等待 done-low 完成闭环。

```text
CHECK_STORE_L2_CSR_ASSERT：
  control service 要求 latest done observation 有效且为 low
  记录 assert_baseline_observation_seq
  enqueue 一个 profile=L2_FLUSH_LEVEL 的 CSR token
  状态转 WAIT_L2_FLUSH_DONE

CSR worker 消费 L2_FLUSH_LEVEL token：
  构造一次 ASSERT item：flush_l2_enable=1，pre_pkt_gap=0，post_pkt_gap=0，metadata=L2_FLUSH_ASSERT + owner
  finish_item() 返回后仅记录 ASSERT sendover；CSR driver 已建立同 owner 的私有 hold
  worker 回到主循环，但当 driver hold 有效且 release queue 为空时只等待 action/shutdown event，
  不得 pop 普通 CSR token；若此时存在不属于该 owner 的 token，直接 uvm_fatal
  driver 在此期间每个 idle sample 按保存 baseline 持续驱动 high，因此 worker 空闲不会形成低电平间隙

WAIT_L2_FLUSH_DONE：
  control service 只接受 observation_seq > assert_baseline 且 level=1 的首个 sample
  标记 done_high 已消费，向 l2_flush_release_request_q 写入带当前 owner、control_reset_epoch 和 release-baseline 的唯一 RELEASE 请求

CSR worker RELEASE：
  control service 将唯一 RELEASE 请求写入 l2_flush_release_request_q 后，同时 trigger csr_control_action_available_ev 唤醒同一 worker
  worker 在 action queue 前优先按 owner 取出 l2_flush_release_request_q；只有当前 driver hold owner 匹配才可继续
  在交付 low 之前记录 release_baseline_observation_seq
  发送一次带同一 owner/epoch 的 flush_l2_enable=0 RELEASE item，finish_item 返回后回写 release sendover，状态转 WAIT_L2_FLUSH_IDLE
  release 后 driver idle 保持 low 合法

WAIT_L2_FLUSH_IDLE：
  control service 只接受 observation_seq > release_baseline 且 level=0 的首个 sample
  标记 done_low 已消费，状态转 CONTROL_COMMIT_READY
```

`flush_l2_enable=1` 的单个 item 不等于保持型请求：当前 CSR driver 在没有 item 时会调用 `drive_idle()` 并把该字段驱回 0，而 DCache responder 在 `DRAIN`/`PROBE` 中观察撤销会 fatal。因此 `L2_FLUSH_LEVEL` 从 ASSERT 到 RELEASE 必须独占 CSR sequencer，且正常 CSR token 不得在该 owner 的 hold 有效时被消费。为使 worker 不直接写 driver 私有状态，在 `csr_ctrl_agent_agent_xaction` 增加非 DUT metadata：`control_action_kind={L2_FLUSH_ASSERT,L2_FLUSH_RELEASE}`、owner(uid/dynamic_epoch/action_generation) 和 baseline-valid 标志。进入 `CHECK_STORE_L2_CSR_ASSERT` 时，service 先确认 control CSR baseline gate 属于当前 epoch，再调用现有 latest API 一次冻结**当前** raw CSR 配置到 L2 token；ASSERT/RELEASE 都基于该 token-local copy 做确定性 raw-to-xaction 映射。这样位于 `check_store` 前的 CSR control 配置不会被回退。映射后强制清零 `satp_changed`、`vsatp_changed`、`hgatp_changed`、`priv_virt_changed` 和全部一次性 CSR write/trigger valid；随后才覆盖 `flush_l2_enable` 与 metadata。这样 L2 flush hold 不会把采样时恰好为高的 CSR change pulse 持续重驱给 DUT。不得把不完整 raw snapshot 或随机未约束字段交给 driver。driver 是 `l2_flush_level_hold` 的唯一写者：收到 ASSERT item 并完成 `send_pkt()` 后深拷贝完整 xaction 为 baseline，校验 owner 后建立 hold；每个 idle sample 读取该 hold，基于保存的 baseline 驱动所有 CSR 字段，仅覆盖 `flush_l2_enable=1`。因此不再产生连续 HOLD xaction，也不让 worker承担 level 保持职责。done-high 后 control service 只把带 owner/epoch/baseline 的 RELEASE 请求写入有界 `l2_flush_release_request_q` 并触发既有 CSR action event；worker 优先从该队列取出后发送 RELEASE item；driver 收到匹配 owner 的 RELEASE item 后驱动 low，`item_done()` 返回后按同一 owner 清 hold，worker 回写 release sendover。旧 owner 的 RELEASE、reset 或 abort 不能清新 generation，均按 owner 校验，否则 `uvm_fatal`。它不改变普通 CSR item 的字段语义，也不引入第二个 CSR driver。`io_mem_to_ooo_topToBackendBypass_l2FlushDone` 只能用于 debug 一致性打印，不能成为第二个推进来源。

## Worker 启动拓扑与 legacy/default sequence 互斥

最终实现采用第二种方式：控制 worker 由专项顶层 VSEQ 显式启动，不使用 agent phase `default_sequence`。`basicTest` 是 active control topology 的唯一 testcase 入口；`memblock_dispatch_real_smoke_vseq` 仅在 AUTO mode 启动两条 worker，`memblock_dispatch_manual_control_vseq` 仅在 MANUAL_CONTROL mode 启动两条 worker。legacy testcase 和无关 VSEQ 不允许 active mode，避免没有 worker 或存在两个 producer 的拓扑。

控制 worker 不能把 `data.is_global_stop_requested()` 作为唯一退出条件：现有 `all_transactions_terminal_done()` 会直接调用 `request_global_stop_if_done()`，而 global stop 又依赖 runtime drain，容易让空 worker 永久等待或让 stop 先于 worker 收敛。新增 `control_worker_topology_active`、`control_workers_shutdown_requested` 和 `control_workers_shutdown_ev`，均由主 control service/拓扑初始化唯一写入。先由 `control_action_drain_complete()`（只检查 active barrier、action queue、owner flushSb completed slot、未闭合 L2 hold，不要求 worker ack）确认动作已收敛；再置 shutdown request 并 trigger shutdown event；CSR/Fence worker 的空队列等待条件固定为“先检查 queue；队列空且 shutdown 未请求时等待 action event 或 shutdown event；收到任一事件后重查”。两个 worker 在确认各自无正在驱动的 token 后写自己的 `worker_exited` acknowledgement。`runtime_drain_complete()` 仅在 `control_worker_topology_active=1` 时要求 shutdown request 和两条 ack；无 control-worker topology 时完全绕过这些条件。必须修改 `all_transactions_terminal_done()`/主 service 的调用顺序，使其先执行 control service 的 drain/shutdown，再允许 `request_global_stop_if_done()` 读取最终 runtime drain；不得用 global stop 反向触发 worker 退出。

最终拓扑固定如下：

```text
basicTest::main_phase：
  创建并显式启动被 +VSEQ_MAIN 选择的顶层 VSEQ

memblock_dispatch_real_smoke_vseq::body（仅 AUTO）：
  通过 p_sequencer.csr_ctrl_sqr 启动 memblock_csr_control_base_sequence
  通过 p_sequencer.fence_sqr 启动 memblock_sfence_control_base_sequence
  同一 VSEQ 并行启动原有 main dispatch、LSQ、issue、L2TLB 和 responder

memblock_dispatch_manual_control_vseq::body（仅 MANUAL_CONTROL）：
  使用同一对 sequencer 显式启动两个 worker
  并行启动专用 direct manual-control main sequence 与既有 responder

worker 主循环：
  worker 在 queue 为空时等待自己的 action event
  main dispatch service 产生 token/request 并消费 monitor/adapter completion
  所有 UID terminal_done、control drain 完成后由主 service 请求 worker shutdown
  worker 收到 shutdown 后确认无 in-flight token，再退出并发布 exited acknowledgement
```

控制标记只由 `build_control_auto_main_table()` 在 mode=`1` 的 AUTO 路径追加；真实 active 入口仅为 `basicTest + memblock_dispatch_real_smoke_vseq`，它在专项 preset 设为 `1` 后生成自动控制表并显式启动 worker。`MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE=0` 时该 VSEQ 保持原有 generic main-table 行为。普通 direct-manual/cancel-reconcile 以 preset mode=`2` 进入其原 builder；`basicTest + memblock_dispatch_manual_control_vseq` 以 mode=`3` 进入控制 builder。`uses_auto_control_barrier_topology()` 只控制自动 `N+1` 建表与随机预约；`uses_control_barrier_topology()` 对 AUTO/MANUAL_CONTROL 都为真，控制 worker、bootstrap、control service 和 shutdown。普通 manual table 和 software-only sequence 即使复用 auto sequence 的 service helper，也不得进入本专项 topology，**但仍必须运行已有 `service_real_dispatch_flow()`**。不得同时保留 legacy/generic CSR/Fence producer 与 VSEQ 显式 worker。

`memblock_dispatch_real_smoke_vseq` 在 active AUTO 时既负责原有 LSQ/issue/L2TLB/主表 orchestration，也负责显式启动 CSR/Fence worker；它不得读取、写入或覆盖 control topology mode。启动前检查 active control topology 的 CSR/Fence 两条 sequencer 非空且未由其他 producer 占用，否则直接 `uvm_fatal`，避免 generic item 与 `L2_FLUSH_LEVEL` 高电平 item 交错。software-only、`tc_dispatch_real_smoke` 与普通 manual directed testcase 不得因本专项自动主表规则被隐式追加 `check_store`；它们请求 mode=`1/3` 时 fail-fast。手工控制只能选择专用 VSEQ 并以 `+MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE=3` 启用完整 control topology。

## Redirect、reset、abort 与超时策略

| 场景 | 首版行为 |
|---|---|
| 控制尚在 `WAIT_OLDER_ROB_COMMIT`，老 UID redirect | 保留静态控制标记，普通老 UID 按既有 reissue；恢复 admission prefix 后重新等待连续 commit。 |
| 控制动作已开始，普通 redirect 覆盖 control ROB | `uvm_fatal`；不得取消、重建或重发 CSR/SFence/`check_store`。 |
| token、`sbIsEmpty`、C0/C4、done observation owner/epoch/generation 不匹配 | 不推进当前状态；若声称属于当前动作但字段矛盾则 `uvm_fatal`。 |
| global/physical 或 L2TLB runtime reset，控制主表已有任一 UID admission | 首版直接 `uvm_fatal` 并 testcase abort；不能只因控制 marker 尚未 active 就保留普通 UID 的旧 status/ROB/LSQ 资源继续运行。只有完整“清主表/ROB 后重建并重启 dispatch”的后续协议才可支持该场景。 |
| bootstrap 或首次 UID admission 之前的 reset | 可清 action queue、event wait state、owner 化 flushSb 生命周期记录、L2 observation 有效位、worker shutdown/exited 状态和未绑定控制临时状态；reset 前 high/完成记录不得复用。 |
| testcase abort，L2 flush 尚未进入 DCache `DRAIN/PROBE` | 按 owner 取消未消费 token/flushSb request，清空本地控制状态。 |
| testcase abort，L2 flush 已在 `DRAIN/PROBE` | 首版 `uvm_fatal` 或要求先走 DUT reset；不得静默拉低 `flush_l2_enable` 后伪造取消。 |
| `flushSb`、控制动作、L2 flush 长时间无完成 | 复用现有 flushSb timeout，并新增控制状态超时诊断；首版只打印足够 owner/state/queue/sample 信息后按专项配置 fatal，不能无限等待。 |

`begin_control_runtime_reset()` 在检查/清理任何 control status 前必须先判定：若 `data.dispatch_progress.max_enqueued_uid_valid=1`、`uid_by_active_rob.num()!=0`、存在 `active_control_barrier_uid` 或任一已绑定 owner，则已有普通或控制 UID 获得运行期 admission/ROB 资源，首版立即 `uvm_fatal` 并请求 testcase abort；不能静默清 control 局部状态后继续使用保留的普通 main table/ROB/LSQ 状态。只有 AUTO 或 MANUAL_CONTROL 主表尚未建立，或主表虽已建立但 `max_enqueued_uid_valid=0` 且所有 active map/barrier/owner 均为空的启动窗口，才允许走 reset 清理和 post-reset ack/ready 协议。清理时必须同时失效 `control_csr_runtime_baseline`，使下一 epoch 的 CSR action 重新等待 CSR monitor 的首份 post-ready snapshot。该 fail-fast 边界保持已有主表/ROB 的 reset 恢复语义不被本专项部分重建逻辑破坏；后续若需支持运行中 reset，必须单独设计“清 main table/ROB 后重建并重启对应 dispatch”的完整协议。

`control_action_drain_complete()` 是不含 worker ack 的前置谓词，只读取 active barrier、CSR/SFence action queue、owner 化 pending/completed `flushSb`、有界 `l2_flush_release_request_q`、未闭合 `L2_FLUSH_LEVEL` 和 in-flight token。`all_transactions_terminal_done()` 或其调用方必须在检测到 terminal prefix 完成后先调用 control service 的 `service_once()`；若 `control_worker_topology_active=1` 且 `control_action_drain_complete()` 成立，则设置 `control_workers_shutdown_requested=1` 并 trigger `control_workers_shutdown_ev`，然后等待两条 worker ack。`runtime_drain_complete()` 必须纳入 active barrier、action queue、owner 化 pending/completed `flushSb`、有界 `l2_flush_release_request_q`、未闭合 `L2_FLUSH_LEVEL`、已请求的 worker shutdown 和两条 worker exited acknowledgement；若 topology inactive，则绕过 worker 条件。只有该最终谓词成立后，`request_global_stop_if_done()` 才能置 global stop；不得用 global stop 反向触发 worker 退出。

## 文件和对象改动清单

| 路径 | 修改类型 | 实施内容 |
|---|---|---|
| `seq/base_seq_help/memblock_dispatch_types.sv` | 修改 | 三个 `op_class`、seq 层 control enums/owner/token/完成事实转换类型/owner 化 `flushSb` 类型；agent/common 只使用 primitive wire metadata。 |
| `seq/base_seq_help/main_control_transaction.sv` | 修改 | 支持 control 主表条目的中性字段校验和打印。 |
| `seq/base_seq_help/status_transaction.sv` | 修改 | control 状态、owner、snapshot、request/event/done baseline 字段及 reset。 |
| `seq/base_seq_help/common_data_transaction.sv` | 修改 | active barrier、queue/event、owner 化 L2 RELEASE request、control admission/retire helper、owner 化 flushSb completion、runtime drain/reset fail-fast。 |
| `seq/base_seq_help/memblock_control_barrier_service.sv` | 新增 | 唯一屏障状态推进、完成事实匹配、CSR/SFence/`check_store` flow。 |
| `seq/base_seq_help/memblock_dispatch_base_sequence.sv` | 修改 | 新增仅供 AUTO 调用的 `build_control_auto_main_table()`（`N+1`、CSR/SFence 预约、末尾 `check_store`）和 control 条目生成/验证/打印；既有 `build_random_main_table()` 保持 DISABLED legacy random 行为；generic `build_main_table()` 仅在 AUTO 校验 legacy manual plus，direct manual/control builder 不经过它；`validate_main_table_entry()` 先走 control-neutral 校验，不能调用普通 behavior。 |
| `seq/base_seq_help/memblock_op_behavior_util.sv` | 修改或保持 guard | 明确普通 behavior 不接收 control；调用方须在其前分流。 |
| `seq/base_seq_help/issue_queue_scheduler.sv` | 修改 | `route_uid()` 和 `prepare_issue_route_for_uid()` 对 control `op_class` 早退，不调用 `derive_op_behavior()` 或生成 issue item。 |
| `seq/base_seq_help/lsq_ctrl_model.sv` | 最小修改 | 如需要，仅提供无 LSQ 分配的 control-admission 辅助；不得把 control 写成 normal behavior。 |
| `seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv` | 修改 | control admission、静态控制 marker rejoin、年轻 admission gate、候选 batch 截断。 |
| `seq/base_seq_help/lsq_commit_handler.sv` | 修改 | control head candidate、control commit/retire、避免控制项的 behavior 推导。 |
| `seq/base_seq/memblock_lsqcommit_dispatch_base_sequence.sv` | 修改 | 调用改名 flushSb API，发送后提交单个 control head。 |
| `seq/base_seq/memblock_main_dispatch_auto_build_main_table_base_sequence.sv` 与新增 manual-control 主 sequence | 修改/新增 | generic main sequence 只接受 plus mode=`0/1`：`0` 保持旧 `build_main_table()`，`1` 进入 `build_control_auto_main_table()`；manual-control sequence 只接受 mode=`3`、通过 direct builder import；两者按 `uses_control_barrier_topology()` 选择 worker/service、共同 post-build bootstrap、物理 reset gate、redirect/reconcile 后 control service 和控制 drain stop。既有 manual/cancel-reconcile sequence 只接受 mode=`2` 且继续运行原 service。 |
| `seq/base_seq_help/dispatch_monitor_event_adapter.sv` | 修改 | 发布并更新按 `lifecycle_event_seq` 索引、可供 control service 消费后清理的 C0/C4 lifecycle completion。 |
| `common/memblock_common/src/memblock_sync_pkg.sv` | 修改 | 定义四值 topology enum，持久保存由 plus 一次冻结的 `control_worker_topology_mode`/派生 active、control-runtime bootstrap/reset epoch、request/ready、driver/producer ack、含 first snapshot seq 的 `control_csr_runtime_baseline` gate，以及 primitive L2 done、`sbIsEmpty`、C0/C4 wire observation/storage/API；禁止引用 seq 层 observation/owner 类型。 |
| `agent/dcache_agent_agent/src/dcache_agent_agent_monitor.sv` | 修改 | 消费 control reset request 并发布 producer ack；每个 ready 后有效 sample 发布带 `control_reset_epoch` 的 `io_l2_flush_done` latest observation。 |
| `agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_monitor.sv` | 修改 | 消费 control reset request 并发布 producer ack；每个 ready 后有效 sample 发布 owner-neutral `sbIsEmpty` latest observation；不直接写 control status。 |
| `agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv` | 修改 | 保持 runtime snapshot 发布职责；消费 control reset request 后发布 producer ack、清 producer-local baseline，并在 ready 后首份 snapshot 发布 `control_csr_runtime_baseline`；不直接写 control status，也不伪造 snapshot reset epoch。 |
| `agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_driver.sv` | 修改 | 作为 `l2_flush_level_hold` 唯一写者，消费 control reset request/发布 driver ack，并消费 xaction 的 ASSERT/RELEASE metadata，完成 baseline 深拷贝、idle 保持、owner 校验和 reset 清理。 |
| `agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_xaction.sv` | 修改 | 增加非 DUT 的 `L2_FLUSH_ASSERT/RELEASE`、owner 和 baseline metadata；不改变 DUT 字段约束。 |
| `agent/fence_agent_agent/src/fence_agent_agent_monitor.sv` | 复用为主 | 保持 raw SFence/C0 采集职责，不直接写 control status。 |
| `seq/base_seq/memblock_csr_control_base_sequence.sv` | 新增 | CSR queue worker、`RUNTIME_CSR_SNAPSHOT` 与 `L2_FLUSH_LEVEL` 驱动。 |
| `seq/base_seq/memblock_sfence_control_base_sequence.sv` | 新增 | SFence queue worker。 |
| `seq/seq.f`、`seq/seq_pkg.sv` | 修改 | 以依赖顺序加入新增 helper/sequence。 |
| `env/plus.sv`、`seq/base_seq_help/seq_csr_common.sv`、`seq/plus_cfg/default.cfg` | 修改 | 新增 `MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE`，并与六个 CSR/SFence 间隔参数一并完成读取、范围校验、getter 和默认 cfg；mode 默认固定为 `0`。 |
| `tc/src/tc_base.sv`、`tc/src/basicTest.sv`、现有 direct-manual/cancel-reconcile 的 `seq/plus_cfg/*.cfg`、新增专项 cfg、`seq/virtual_sequence/memblock_dispatch_real_smoke_vseq.sv`、`seq/virtual_sequence/memblock_dispatch_manual_control_vseq.sv` | 修改/新增 | `tc_base`/`basicTest` 从 `seq_csr_common` getter 冻结 plus mode snapshot；只有 `basicTest` 的两个专项 VSEQ 经 `p_sequencer` 显式启动 CSR/Fence worker。legacy testcase 和非允许 VSEQ 在 mode=`1/3` 入口 fail-fast；manual/cancel preset 显式写 `2`，AUTO/手工控制专项 preset 分别写 `1/3`。主 sequence 只读取 snapshot 并校验 builder/mode 匹配，保留所有既有 manual dispatch service。 |

任何新增 sequence/testcase 都必须按现有 `seq.f`/`seq_pkg.sv`/`tc.f`/`tc_pkg.sv` 管理顺序加入；参数按现有公共 plus 参数路径管理，不能把 runtime status 伪装成 plus 参数。

## 实施顺序

1. 先添加类型、控制 status、owner 比较/helper 和参数；只完成编译期可见性，不接入运行期。
2. 接入 `MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE` 的 plus 读取、范围校验、不可变 snapshot、mode/builder mismatch fail-fast、worker shutdown/ack 和 VSEQ 显式 worker 的 producer 互斥；先验证 mode=`0` 的 legacy testcase 与 mode=`2` 的现有 direct-manual testcase 均不启动 worker，`basicTest` 的 mode=`1/3` 空控制队列也能自然结束。
3. 修改自动建表为 `N+1` 与三个 `op_class`，先增加 structural dump/assertion：CSR/SFence 只在 `[0,N)`，UID `N` 必是 `check_store`，ROB 连续。
4. 接入 control-active admission、屏障 gate、静态 redirect preserve/rejoin；保持 action queue 为空时不允许越过控制 UID。
5. 接入 control commit/retire 分支，使一个人工置为 `CONTROL_COMMIT_READY` 的 control UID 能完整 `terminal_done`，验证 cursor/ROB map 不受影响。
6. 扩展 owner 化 `flushSb` 请求/attached/sendover/completion 和 `sbIsEmpty` latest observation，先完成 SFence 与 `check_store` 共用的 `WAIT_SB_EMPTY` 新鲜度闭环。
7. 新增 CSR/SFence worker 和 CSR runtime snapshot 完成；先完成单个 CSR control 标记。
8. 接入 SFence C0/C4 精确 observation，验证 C4 前不能 commit/释放。
9. 接入 DCache done latest observation、owner 化 `l2_flush_release_request_q` 和 `L2_FLUSH_LEVEL` driver hold/high/release/low 闭环，最后启用自动末尾 `check_store`。
10. 完成 directed testcase、timeout dump、文档和编译/仿真验证后再扩大随机间隔场景。

每一步均应保持普通 load/store 自动主表与手工主表的既有行为不变；每一步先通过编译和最小 directed case，下一步不以隐藏 previous failure 的方式继续。

## Directed 验收场景

| 场景 | 配置或构造 | 关键验收点 |
|---|---|---|
| 固定末尾保留 | `MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE=1`、`N=100`、CSR/SFence 关闭 | 实际 `main_trans_num=101`，UID 100 唯一 `check_store`，ROB 连续。 |
| CSR 间隔 | mode=`1`，CSR enable，`min=max=15` | UID 15 为 CSR，snapshot 必须来自 monitor，control terminal 后才放行 UID 16。 |
| SFence 间隔 | mode=`1`，SFence enable，固定目标 | topology 已启用且唯一 L2TLB lifecycle owner 已建立；`flushSb` attached、driver sendover、sendover 后的新鲜匹配 `sbIsEmpty`、pre-drive baseline 后（含同拍）的 C0、C4、commit、terminal 顺序完整且不能跳步。 |
| SFence 无 dispatch | mode=`1`、`MEMBLOCK_SFENCE_CONTROL_ENABLE=1`，但选择 no-dispatch 场景 | 建表/topology 初始化阶段依据 plus 冻结的静态 mode 与实际 dispatch capability 的不匹配直接 `uvm_fatal`，不能静默丢 raw fence 后永久等待。 |
| SFence adapter owner 缺失 | dispatch active 但 `l2tlb_adapter_service_active`、注册 owner 或 lifecycle owner 缺失 | 控制动作启动前直接 `uvm_fatal`，不能进入 `SFENCE_REQ`。 |
| SFence L2TLB baseline 等待 | owner 已建立但 `l2tlb_post_reset_baseline_done(current_epoch)=0` | 保持 `SFENCE_REQ`，不 arm/drive；baseline proof 到达后才入 action queue，超时走控制状态诊断。 |
| 同目标优先级 | CSR/SFence 都固定同一目标 | 当前 UID 为 CSR，SFence 旧目标被消费并重新预约，不重复命中。 |
| 越界目标 | `min=max >= N` | 不生成 CSR/SFence，UID N 仍是 `check_store`，不扩表。 |
| redirect 静态等待 | 控制 UID 10，UID 8 redirect 至 5 | UID 10 不产生 token/request，UID 5..9 重发后才启动控制动作。 |
| redirect 覆盖动作 | CSR/SFence/check_store 已开始后构造覆盖其 ROB 的 redirect | 专项 `uvm_fatal`，不得 silent reissue。 |
| `check_store` L2 level | 所有前序完成后触发末尾标记 | `flushSb -> sbIsEmpty -> high hold -> done high -> release -> done low -> commit`，高电平期间无 CSR idle gap。 |
| worker 互斥 | 启动 mode=`1` 或 mode=`3` 的专项 testcase | CSR/Fence sequencer 都只存在 control worker；没有 legacy/generic item 与控制 token 并发。 |
| worker 退出 | 自动 real-dispatch 场景中 CSR/SFence 均未命中 | 主 service 先完成不含 ack 的 control action drain，再置 topology active 的 shutdown request 并 trigger event；两个空闲 worker 均发 exited acknowledgement；最后才允许 global stop，phase 能自然结束。 |
| control reset release | reset 中释放 control request，随后首个 post-reset sample | 外层 reset gate 先发 request；CSR driver 清私有 hold 并 ack；CSR/DCache/ctrl 首个 sample 先发 producer ack，三类 ack 齐备后 complete，下一 sample 才发布可消费 observation；ready 前无 completion/token 消费。 |
| control-runtime bootstrap | 初始 `rst_n` 已经释放、尚未出现物理 reset tick 时启动 mode=`1` 或 mode=`3` topology | 任一控制主表构建返回后、service 启动前共同 hook 一次性建立 epoch/request；CSR driver 与 CSR/DCache/ctrl 以观察到该 request 后的首个 sample 回 ack，四个 ack 齐备后 ready；CSR monitor 的下一 sample 发布本 epoch `control_csr_runtime_baseline.first_snapshot_seq`，CSR token 必须在此后从 current latest 冻结自己的 baseline。 |
| SFence 同拍 C0 | driver 交付 SFence 与 Fence monitor 在同一采样边界产生 C0 | `start_item()` 前冻结 `last_allocated_l2tlb_event_seq` 并置 `sfence_c0_match_armed`，同拍新 event 以更大 seq 被记录；`finish_item()` 后 service 才消费；不得用 finish 后 baseline 或仅匹配 sendover。 |
| reset after admission | mode=`1/3` 任一普通或控制 UID 已 admission，随后触发 global/physical 或 L2TLB runtime reset | 立即 `uvm_fatal`/testcase abort；不得仅因 control marker 尚未 active 而清局部状态后继续跑旧 main table/ROB。 |
| topology 隔离 | software-only、普通 manual real dispatch、无关 `basicTest` vseq，分别使用 mode=`0/2` | 不替换 generic/default CSR/Fence producer，也不等待 control worker shutdown；mode=`3` 是显式例外，必须启动完整 control topology。 |
| DISABLED 控制条目 | mode=`0` 且 legacy generic/manual import 中显式构造 CSR/SFence/check_store op_class | post-build 立即 `uvm_fatal`；不得因无 control worker 而进入静默等待。 |
| direct-manual 派生隔离 | `tc_dispatch_real_mixed_wb_smoke`/mixed STA 派生或 `memblock_dispatch_real_cancel_reconcile_vseq`，其 preset mode=`2` | 保留其既有 manual builder 和 generic CSR/Fence default；不得因继承 real-smoke 类/VSEQ 而进入 AUTO 或追加 `check_store`。 |
| mode 非法值 | `MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE=-1` 或 `4` | `seq_csr_common::validate_and_clamp()` 在 build 前 `uvm_fatal`；不得 clamp、忽略或回退。 |
| mode/sequence 不匹配 | generic main sequence 配 mode=`2/3`，或 direct-manual sequence 配 mode=`0/1`，或 direct manual-control sequence 配非 `3` | 对应 main sequence body 入口 `uvm_fatal`；不得由 testcase/VSEQ 重写 plus 或切换 builder。 |
| 长度边界 | `N=1`、`N=10000` | `N+1` 分配不溢出，control service 只处理单 barrier，无每拍全表扫描。 |

验证采用 V2 远端流程：先在 `mem_ut/ver/ut/memblock/sim` 执行对应 testcase 的 `make eda_compile`，再执行 `make eda_run`；最终日志需满足 `TEST CASE PASSED`、`UVM_ERROR=0`、`UVM_FATAL=0`。涉及预期 fatal 的 negative testcase 应单独把 fatal 视为命中条件，不能混入正常 smoke 通过标准。

## Coding 前主审确认项

1. 本 plan 把 `MEMBLOCK_MAIN_TRANS_NUM=N` 定义为固定普通区间长度而非启用控制后仍保证存在的真实访存条数；这是保留 `UID=N` 固定 `check_store` 的必要解释。
2. `check_store` 只由 `build_control_auto_main_table()` 在 `MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE=1` 时追加，不能回写到既有 `build_random_main_table()`；对应 AUTO 专项 cfg 必须按 mode 配置 control worker，不能只给单个新 testcase 补 worker 后让其他 AUTO 流卡住，也不能把 worker 无条件装入不使用 control main table 的 testcase。手工控制条目必须以 mode=`3` 进入 `MANUAL_CONTROL_MAIN_TABLE`，普通 mode=`2` 不允许悄然承载控制条目。
3. `flush_l2_enable` 的 level hold 由 CSR driver 的唯一 `l2_flush_level_hold` 维持，不依赖 CSR worker 连续 high item 交付；coding 时必须用 waveform/monitor 日志验证 ASSERT 到 RELEASE 之间所有 CSR driver idle sample 仍保持高电平。
4. C4 completion 必须由既有 adapter 实际消费 lifecycle event 后发布，而不是仅凭控制状态机计算到期 sample。
5. 当前 runtime CSR snapshot 只在 payload changed 时递增。初版配置函数必须保证 monitor 可见变化；no-op CSR 配置属于后续扩展，需要新增独立 acknowledge 或每拍观察序号。
6. `check_store` 的 L2 flush 若已进入 DCache `DRAIN/PROBE`，普通 abort/redirect 的取消协议目前不存在；首版必须保持 fatal/reset 边界，不能伪造安全取消。
7. `sbIsEmpty` 完成必须以 sendover 后的新 observation 为准；不可仅以 attached、active request 或已有 high level 推进。deferred raw 必须携带 immutable observation seq，completed owner slot 消费后立即清理。
8. `L2_FLUSH_LEVEL` hold 必须保存 owner 与完整 CSR baseline；done-high 到 RELEASE 的交接使用有界 owner 化 `l2_flush_release_request_q`，driver idle、RELEASE、reset 都按 owner 校验，不能只用一个无主 boolean 或瞬时 event。
9. CSR runtime snapshot 不携带 reset epoch；完成条件只使用 reset generation 边界和 `snapshot_seq > baseline`。L2 done 使用独立 `control_reset_epoch`。
10. `dispatch_raw_csr_t` 不是完整 CSR agent xaction。首版配置函数必须构造不 randomize 的完整 xaction、显式映射 monitor 可见字段并固定剩余安全字段；ASSERT/RELEASE 复用同一映射，避免未约束字段随机改变 DUT CSR 输入。
11. C0/C4 只能匹配 `l2tlb_current_reset_epoch`；`control_reset_epoch` 只保护 CSR/`sbIsEmpty`/L2 done 控制事实，二者不得互相替代。
12. `control_runtime_ready` 只允许 producer 开始发布当前代 observation，不等于当前 CSR snapshot 已可用；CSR action 必须再等 `control_csr_runtime_baseline`，并冻结其对应 seq/payload 后才入 action queue。

## 执行中补充/修正（IMPLEMENTATION_DELTA）

### [IMPLEMENTATION_DELTA] control worker 改为 VSEQ 显式启动

来源：coding 前读取 `AI_DOC/project_management/mem_ut_virtual_sequence_rule.md` 后发现，原 plan 以 testcase phase `default_sequence` 安装 CSR/Fence control worker 的方式，与当前“agent sequence 由顶层 VSEQ 经 `p_sequencer` 启动”的项目规则冲突；用户已明确要求改为第二种方式，并要求保留其它控制入口支持。

原 plan：`tc_base`、`basicTest` 和 `tc_dispatch_real_smoke` 在 build/config 阶段按 topology mode 覆盖 CSR/Fence sequencer 的 `main_phase.default_sequence`，由 phase 自动启动 `memblock_csr_control_base_sequence` 与 `memblock_sfence_control_base_sequence`。

实现调整：两个 worker 的实现类、token/event 协议和 shutdown acknowledgement 保持不变，但不再作为 phase `default_sequence` 安装。`memblock_dispatch_real_smoke_vseq` 在 AUTO topology 下使用 `p_sequencer.csr_ctrl_sqr` 和 `p_sequencer.fence_sqr` 显式并行启动两个 worker；新增 MANUAL_CONTROL 专项 VSEQ 复用相同 worker 启动路径并启动专用 manual-control main sequence。`basicTest` 是这两个 active VSEQ 的唯一 testcase 场景入口；已有 `tc_dispatch_real_smoke` 及其它 legacy testcase 在 mode=`1/3` 时直接 fail-fast，而不再尝试间接启动 real-smoke VSEQ。普通 direct-manual/cancel-reconcile testcase 保持 mode=`2` 的既有 default topology；它们若请求 active control mode 同样在入口 fail-fast，避免出现无 worker 的控制表。

原因：VSEQ 是场景内 agent sequence 的唯一生命周期 owner；将 worker 与 main dispatch、LSQ、L2TLB 和 responder 放在同一 VSEQ 并发域，才能保证每个 sequencer 只有一个 producer，并符合当前 VSEQ 调度规则。

影响范围：修改 worker 启动/关闭责任、`basicTest` 与 legacy real-smoke 的 mode 兼容检查、VSEQ 入口和相关 preset；不改变 plus 为唯一 topology 输入、控制标记/ROB/CSR/SFence/flushSb/L2 flush 的主体状态机，也不改变普通 mode=`0/2` 的 legacy 行为。

### [IMPLEMENTATION_DELTA] 主 dispatch helper 在建表入口保证初始化

来源：专项 `basicTest + memblock_dispatch_real_smoke_vseq` 首次运行显示，`uvm_do_on()` 以 `call_pre_post=0` 启动其 child main sequence，因此 `memblock_dispatch_base_sequence::pre_body()` 不会执行。新加入的 `control_barrier_service` 原本仅在该回调中构造，AUTO 建表完成后调用 bootstrap 时发生空对象访问。

原 plan：主 dispatch sequence 的 `pre_body()` 在建表前完成 data、LSQ runtime、handler、adapter 与 control barrier service 的初始化；后续 `build_main_table()` 直接进入 AUTO/legacy 分流。

实现调整：新增幂等的 `ensure_dispatch_runtime_helpers()`。`pre_body()` 与 `build_main_table()` 都调用它；首次调用统一构造上述 helper、复位本轮 LSQ runtime，并在 active topology 时注册现有 L2TLB adapter service。函数不建表、不分配 UID、不推进控制状态。这样显式 VSEQ child 启动与未来直接 `start(..., call_pre_post=1)` 的入口得到相同初始化结果。

原因：保持项目规定的 VSEQ 经 `uvm_do_on` 调度方式，不为解决单个 helper 初始化而修改所有 child sequence 的 `call_pre_post` 语义；同时避免控制 service 与既有 dispatch helper 走两套初始化路径。

影响范围：`memblock_dispatch_base_sequence` 的内部初始化边界和专项仿真；不改变任何 worker、ROB、CSR、SFence、flushSb 或 L2 flush 对外状态机。

## 与初步 plan 差异说明

本章只记录相对于两份 flow 草案和现有框架行为的功能实现差异，coding 时以正文前述最终 flow 为准。

### 差异一：`flushSb` 从单一“驱动”事实拆为请求生命周期

修改目的：现有 `mark_flushsb_driven()` 在 `start_item/finish_item` 前调用，无法区分队列消费、xaction 附加和 driver 交付；同时 deferred raw 只有可变的 latest level，可能把后续 sample 错归给旧请求。

修改前文字伪代码：

```text
control service 调用 push_flushsb_request()：只把无 owner request 放进 flushsb_req_q
LSQ commit sequence try_pop_flushsb_request()：pop 后立即调用 mark_flushsb_driven()
mark_flushsb_driven()：立刻置 active request、flushsb_waiting_empty 和 monitor capture gate
deferred ctrl 消费：把 raw.sb_is_empty 作为 bit 传给 update_sb_is_empty()
update_sb_is_empty(1)：直接清 active request
```

修改后文字伪代码：

```text
enqueue_control_flushsb_request(owner)：
  检查当前没有 active/attached/completed owner request
  调用 push_owner_flushsb_request(owner, req)，由公共 helper 分配 req_id、写入 owner 字段并入队
  把同一 req 保存到控制 status，状态保持在 *_FLUSHSB_PENDING

LSQ commit consumer：
  try_pop_flushsb_request() 只允许在没有 active request 时 pop
  将 req 附加到当前 lsqcommit xaction
  在 start_item() 前调用 mark_flushsb_request_attached_to_lsqcommit_xaction()
  finish_item() 返回后调用 mark_flushsb_request_driver_sendover()
  只有第二个 helper 才打开 waiting-empty 状态，并记录 sendover baseline observation seq

ctrl monitor：
  每个有效 sample 更新 owner-neutral latest sbIsEmpty observation
  raw ctrl 同时冻结 sb_is_empty、sb_is_empty_observation_seq 和 cycle
  raw 进入 raw/deferred FIFO 后不得被消费时的 latest observation 覆盖

apply_raw_ctrl_deq(raw)：
  把完整 immutable raw 传给 update_sb_is_empty(raw)
  只有 raw level=1、seq 大于 sendover baseline、active request 已 sendover 时才完成
  owner request 写入单个 completed slot；control service 按 req_id+owner 消费后立即删除
```

差异影响：请求登记和 driver 交付的接口语义更精确，但仍复用原有 `flushsb_req_q`、LSQ commit consumer、ctrl monitor 和 deferred service；legacy 无 owner request 继续使用旧直接完成语义。completed slot 有界且有明确删除点，不会阻塞 `runtime_drain_complete()` 或造成无界增长。

新增 helper 详细文字伪代码：

```text
push_owner_flushsb_request(owner, req)：
  输入 owner；输出完整 req
  校验 owner 有效、无 pending owner completed slot
  分配单调 req_id，写 enqueue_cycle/source/owner 字段并 push 到公共 queue
  返回 req；失败使用 uvm_fatal

update_sb_is_empty(raw)：
  输入冻结的 raw level/observation seq/cycle
  先更新 latest debug 字段
  若无 active request、尚未 sendover 或 seq 不新，则不消费
  若 level=1 且 owner request 匹配，则复制 completed slot、清 active request 和 waiting gate
  legacy request 沿用旧清理分支；control service 负责 owner slot 的最终删除
```

### 差异二：静态控制标记不参与 redirect rollback

修改目的：静态 `WAIT_OLDER_ROB_COMMIT` 控制标记虽然可能落在 redirect 的 ROB 范围内，但它不是被取消的动态访存实例。若将其 UID 计入 `oldest_flushed_uid`，现有 `rollback_max_enqueued_uid()` 会错误回退到保留的控制 UID。

修改前文字伪代码：

```text
apply_redirect_flush_range(redirect)：
  扫描 active window
  对所有 rob_need_flush(uid) 先更新 oldest_flushed_uid
  再调用 prepare_uid_for_redirect_reissue(uid)
  用 oldest_flushed_uid 回退 max_enqueued_uid
```

修改后文字伪代码：

```text
扫描 active window：
  若 uid 是静态 control marker：只保留 active ROB map、barrier 和 WAIT_OLDER_ROB_COMMIT
    不更新 found_flushed/oldest_flushed_uid，不记 cancel 计数，不调用普通 reissue
  若 uid 是普通访存：按既有 prepare_uid_for_redirect_reissue() 重建动态实例
    只有该 UID 才参与 oldest_flushed_uid
  若 uid 是已开始动作且 redirect 覆盖其 control ROB：uvm_fatal
若存在真正被 flush 的普通 UID：调用 rollback_max_enqueued_uid(oldest_flushed_uid)
```

新增 helper 详细文字伪代码：

```text
preserve_static_control_marker_on_redirect(uid, redirect)：
  输入控制 UID 和 redirect
  校验 status 仍是静态 WAIT_OLDER_ROB_COMMIT 且 control-active map 未丢失
  保持 barrier/robIdx/dynamic_epoch 不变，返回“保留且不计入 rollback”
  若状态已离开静态等待且 redirect 覆盖 control ROB，uvm_fatal
```

### 差异三：SFence C0/C4 改为可查询、可清理的 lifecycle record

修改目的：现有 adapter 的 `sfence_invalidate_pending_q` 在 C4 会 pop，函数只返回删除数量；control service 若错过同一 tick，就无法证明某个 token 的 C4 已实际生效。

修改前文字伪代码：

```text
schedule_sfence_invalidate()：把 pending work 放入 due queue
apply_due_sfence_invalidate()：到 due sample pop pending，扫描 live entry 并返回 deleted_count
control service：尝试从 pending queue 为空推断 C4
```

修改后文字伪代码：

```text
schedule_sfence_invalidate(payload,event)：
  校验 sample/reset/event provenance
  仅当存在可匹配的 armed 或 SFENCE_SENDOVER token 时写入有界 lifecycle_observation[event]，状态为 C0_SCHEDULED/claimed；armed token 允许同拍 C0 先落 record
  generic/manual fence 不创建 control record，只进入既有 C4 due queue
  将 pending work 放入既有 due queue

apply_due_sfence_invalidate(sample)：
  按既有 due queue 执行 bounded live-entry 删除
  无论删除数量是否为 0，都把 claimed lifecycle_observation[event] 更新为 C4_EFFECTIVE
  保存 due sample、l2tlb_reset_epoch、payload 和 deleted_count

control service：
  只按 token 的 event id、payload、l2tlb_reset_epoch 查询 C0/C4 record
  C4_EFFECTIVE 被当前 owner 消费后删除 record
  unclaimed generic record 不保留；reset 或过期记录清理整张有界 map；超过容量属于框架状态错误并 fatal
```

SFence control 建动作前还必须检查 `dispatch_l2tlb_lookup_active`、adapter service 注册状态和唯一 L2TLB lifecycle owner；no-dispatch topology 下直接 fatal，不允许 raw fence 被静默丢弃后永久等待。

### 差异四：CSR snapshot 与 L2 flush 使用不同 reset/completion 语义

修改目的：现有 `runtime_csr_snapshot` 没有 `reset_epoch` 字段且 reset 不清 latest snapshot；`flush_l2_enable` 也不属于 runtime snapshot changed 语义。将两者混用会产生旧 snapshot 或旧 level 误完成。

修改前文字伪代码：

```text
CSR drive 完成后读取 latest snapshot
若 payload 看起来匹配则保存为完成 snapshot
对 flush_l2_enable 也沿用同一 snapshot 条件
```

修改后文字伪代码：

```text
普通 CSR：
  `CSR_CONFIG_PENDING` 先等当前 control epoch 的 control_csr_runtime_baseline
  将该 monitor baseline 冻结到 token 后才 drive，并记录 snapshot_seq_before_drive
  只接受 seq > baseline 且 payload changed/expected 匹配的 monitor runtime snapshot
  reset generation 变化或无新 seq 时不完成；snapshot 本身不读取不存在的 reset_epoch

L2_FLUSH_LEVEL：
  使用独立 control_reset_epoch 和 done observation seq
  ASSERT/RELEASE baseline 与 high/low observation 必须同一代且 seq 新于 baseline
  不把 CSR runtime snapshot 当作 L2 flush 完成证据
```

### 差异五：L2 flush hold 从无主布尔量改为 owner 化状态

修改目的：当前 CSR driver 无 item 时必经 `drive_idle()`，且没有完整 CSR baseline cache；仅设置一个 boolean 无法防止旧 RELEASE/reset 清掉新 generation，也无法定义 idle 时其余字段。

修改前文字伪代码：

```text
worker 发送一个 flush_l2_enable=1 item
driver 没有 item 时 drive_idle()，把 flush_l2_enable 拉低
```

修改后文字伪代码：

```text
ASSERT：
  worker 构造一次完整 CSR baseline，清 CSR changed/write pulse，并在 xaction 非 DUT metadata 写入 L2_FLUSH_ASSERT + owner
  driver 完成 send_pkt() 后深拷贝完整 xaction 为 baseline，并由 driver 建立 l2_flush_level_hold(valid=1, owner, release_requested=0)

HOLD：
  worker 不再循环发送高电平 xaction；CSR sequencer 保持该 owner 独占并等待 RELEASE 请求
  driver 每个 idle sample 读取 hold，按保存的完整 baseline 驱动所有字段，仅覆盖 flush_l2_enable=1
  owner 不匹配或 baseline 无效时 fatal

RELEASE：
  done-high 被当前 owner 消费后，worker 在 xaction metadata 写入 L2_FLUSH_RELEASE + 同一 owner
  driver 校验 owner、驱动 low；item_done() 返回后按同一 owner 清 hold
  reset/abort 只在协议允许的边界清 hold；DRAIN/PROBE 中的非法撤销仍 fatal
```

### 差异六：worker shutdown 与 global stop 解耦

修改目的：现有 `all_transactions_terminal_done()` 直接请求 global stop，而 worker 退出又等待 stop，存在循环依赖；没有 control marker 的 testcase 也不应等待不存在的 worker ack。

修改前文字伪代码：

```text
terminal_done_prefix 到达末尾
all_transactions_terminal_done() -> request_global_stop_if_done()
worker 以 global_stop 作为退出条件
```

修改后文字伪代码：

```text
terminal_done_prefix 到达末尾后：
  主 service 先执行最终 service_once()
  若 topology active 且 control_action_drain_complete() 成立：置 shutdown_requested 并 trigger shutdown_ev
  worker 醒来后重查 queue；无 in-flight token 时写 worker_exited ack 并退出
  topology inactive 时跳过 worker shutdown/ack 条件
  只有最终 runtime_drain_complete() 成立后才调用 request_global_stop_if_done()
```

差异影响：普通 manual/software-only topology 保持原 default sequence 和 stop 语义；自动 control topology 增加明确的 worker 生命周期，但不增加第二个 producer。

### 差异七：L2 flush 高电平由 driver hold 单一维护

修改目的：若 CSR worker 持续发送 high item，同时 CSR driver 又在 idle 周期保存同一 high level，就会存在两个组件维护同一 `flush_l2_enable` 语义；worker 调度、release event 和 driver idle 的边界会被不必要地耦合。

修改前文字伪代码：

```text
worker：
  ASSERT 后循环发送 HOLD high item，避免 driver idle
driver：
  可能还按私有 hold 在 idle 周期维持 high
```

修改后文字伪代码：

```text
worker：
  只发送一次 ASSERT item，并在 owner hold 有效期间独占 CSR sequencer
  done-high 后由 RELEASE queue + action event 唤醒，发送一次 RELEASE item

driver：
  ASSERT send_pkt 完成后建立唯一 l2_flush_level_hold
  每个无 item 的 sample 使用该 hold 维持 high
  匹配 RELEASE send_pkt 完成后驱动 low 并清同 owner hold
```

差异影响：`flush_l2_enable` 的持续高电平只有 driver 一个写者，worker 仍保留 token、ASSERT/RELEASE 的 sequence 所有权和 sequencer 独占。该修正不改变草案要求的 `flushSb -> done-high -> release -> done-low -> commit` 顺序，但减少了高频 item 创建和竞态面。

### 差异八：控制拓扑改由专用 plus 统一选择

修改目的：当前 `build_main_table()` 仅以 `MEMBLOCK_USE_MANUAL_MAIN_TABLE` 选择随机建表或导入手工表；它并不知道 CSR/SFence worker、控制标记或 `check_store`。此前方案再由 testcase/VSEQ 写 mode，会让场景入口同时拥有“选择 sequence”和“选择控制拓扑”两项责任，且 direct-manual 派生场景容易因继承关系写错 mode。需要把四个互斥 mode 收敛为公共 plus 输入，同时继续保留 legacy manual plus 的原始建表含义。

修改前文字伪代码：

```text
testcase/VSEQ：
  选择场景与 main sequence
  testcase getter 或 VSEQ allowlist 再写 control topology mode

main sequence：
  读取 testcase/VSEQ 写入的 mode
  再读取 MEMBLOCK_USE_MANUAL_MAIN_TABLE 决定 generic build 分支
  direct-manual 派生路径需要额外覆写 mode，避免继承 AUTO
```

修改后文字伪代码：

```text
testcase build/config：
  plus.sv 读取 MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE
  seq_csr_common 校验值只能为 0..3，并返回对应枚举
  initialize_control_worker_topology_from_plus() 将该值冻结到 memblock_sync_pkg
  testcase 不安装 CSR/Fence control worker；VSEQ 也不写 mode
  仅 basicTest 的两个允许 VSEQ 可在 mode=1/3 使用 frozen snapshot 显式启动 worker
  reset_all_tables() 不清 snapshot

generic main sequence：
  mode=0：执行既有 generic build_main_table()，允许 legacy manual plus 维持旧 random/import 分流，但 post-build 拒绝控制 op_class
  mode=1：校验 legacy manual plus 为 0，调用 build_control_auto_main_table(N)
  mode=2/3：直接 fatal，要求使用各自 direct main sequence

MANUAL / MANUAL_CONTROL direct sequence：
  只接受 mode=2 或 mode=3
  直接调用各自 manual builder 和 import，不读取 legacy manual plus
  MANUAL 拒绝所有控制 op_class；MANUAL_CONTROL 只接受显式构造的控制标记
  两类路径都继续进入已有 service_real_dispatch_flow()
```

差异影响：`MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE` 成为控制 worker、bootstrap 和屏障服务的唯一配置源；`VSEQ_MAIN` 只负责场景选择，不能再改变 mode。默认值 `0` 保留未更新 testcase 的 legacy 行为；现有 direct-manual/cancel-reconcile preset 显式升级为 `2`，AUTO/手工控制专项 preset 分别使用 `1/3`。自动控制表仍 fail-fast 防止误走 legacy manual plus 分支；普通手工和手工控制 builder 均不会因继承链或 VSEQ 名称而静默切换控制语义。

### 差异九：CSR action 使用当前 control epoch baseline，L2 profile 不重驱 CSR pulse

修改目的：`control_runtime_ready` 只表示 CSR/DCache/ctrl producer 已完成当前 epoch 的接入确认；其后的首个 CSR sample 只证明 global latest 已进入当前 epoch，而每个 CSR/L2 action 都必须保留自己的当前 monitor 配置。并且原始 snapshot 中的 `*_changed`/write valid 可能是一次性 pulse，L2 flush hold 不能把它们持续保持。

修改前文字伪代码：

```text
control runtime ready 后：
  CSR worker 直接读取 global latest snapshot 构造 action

L2 ASSERT/HOLD：
  从 raw snapshot 复制字段，只把 flush_l2_enable 改为 1
  若 raw 中有 changed/write pulse，也随 hold 一直驱动
```

修改后文字伪代码：

```text
CSR monitor：
  收到 control_reset_request 的 ack sample 只确认 producer 已进入 epoch，并清自己的 local baseline
  ready 后首个实际 runtime snapshot 发布时，写 control_csr_runtime_baseline(epoch, snapshot_seq)

CSR_CONFIG_PENDING：
  等 baseline 的 epoch 等于当前 control_reset_epoch
  从 global latest 一次读取 raw+seq，要求 seq >= first_snapshot_seq
  冻结本 action 的当前 payload 到 token，再入 CSR action queue

普通 CSR configure：
  只保留本 action 的 satp_changed=1
  清 vsatp/hgatp/virt changed 与所有一次性 write/trigger valid，避免重驱 baseline sample 中的旧 pulse

L2 ASSERT/RELEASE：
  在 ASSERT 前以同一 gate 从 global latest 一次冻结当前 CSR 配置
  用该 action-local copy 做确定性 raw-to-xaction 映射构造完整 baseline
  清 satp/vsatp/hgatp/virt changed 与所有一次性 write/trigger valid
  只覆盖 flush_l2_enable 和 owner metadata；driver hold 仅保持这一稳定 baseline
```

差异影响：普通 CSR 仍只以 monitor 观察到的新 snapshot 完成；`check_store` 仍只以 L2 done high/low 完成。新增 epoch gate、每 action current-snapshot freeze 和 pulse 清零只排除 bootstrap 早到旧 snapshot、较早 CSR 配置被回退、以及无关 CSR pulse 被 level hold 重复驱动的实现风险，不改变草案顺序。

### 新增 reset helper 详细文字伪代码

```text
begin_control_runtime_reset(reset_reason)：
  抽象功能描述：由主 control service 在 reset 边界唯一调用，先拒绝任一 UID 已 admission 的运行中 reset，再对尚未 admission 任一 UID 的启动窗口递增控制 reset epoch 并清理不能跨 reset 复用的控制事实；不修改主表 UID/ROB 分配。
  输入 reset reason、当前 control topology；输出新的 control_reset_epoch
  若本次 reset 已处理则返回当前 epoch
  若 max_enqueued_uid_valid、active ROB map、active_control_barrier 或已绑定 owner 任一存在：输出 uid/owner/state 后 uvm_fatal 并 abort；不得清局部控制状态后继续
  递增 control_reset_epoch
  清未绑定控制临时状态、action queue、l2_flush_release_request_q、owner flushSb active/completed slot、C0/C4 observation map、L2 done valid、control_csr_runtime_baseline 的 valid/first_snapshot_seq 和 worker ack
  置 control_reset_request=1、control_runtime_ready=0；不直接写 CSR driver 私有 l2_flush_level_hold
  CSR driver 在自身 reset/观察 request 后按 owner 清 hold，完成后发布 driver_reset_ack(epoch)

initialize_control_runtime_bootstrap()：
  抽象功能描述：AUTO 或 MANUAL_CONTROL 主表在建表完成后经共同 hook 唯一调用的启动初始化；建立首个 control reset epoch 和 request，清理旧的完成观察，但不改主表、不创建 token。
  若 bootstrap 已完成则直接返回
  设置 `memblock_sync_pkg` 中 control_reset_epoch=1、control_reset_request=1、control_runtime_ready=0，清理 action/release queue、owner completion 和 producer ack 槽；`reset_all_tables()` 不得清除这些字段
  允许 CSR driver、CSR/DCache/ctrl monitor 在观察到该 request 后分别发布当前 epoch 的 ack
  service 收齐 driver ack 与三个 producer ack 后调用 complete_control_runtime_reset()

complete_control_runtime_reset(sample_seq)：
  抽象功能描述：在首个 post-reset producer ack 且 topology ready 后开放新的控制事实消费；不把旧 latest snapshot 当作新动作完成。
  输入 sample_seq、topology readiness；输出 control_runtime_ready
若 reset 仍 active、topology 未完成注册、CSR driver reset ack 未到，或三个 `post-reset producer ack` 未齐则保持 not-ready；不能把可消费 observation 作为 ready 前置条件
主 service 在 reset release 后先收集三个 producer ack，随后以 ack 对应的 sample 调用本 helper
首次满足条件时清 control_reset_request、置 control_runtime_ready=1；从下一 sample 起 monitor 才发布带当前 control_reset_epoch 的可消费 observation
  CSR action 保存该 epoch；L2 done high/low、hold owner 和 baseline 只接受同 epoch 事实
```
