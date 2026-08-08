# mem_ut V2 `isStoreException` Agent 迁移与驱动适配 Plan

| 项目 | 内容 |
|---|---|
| 状态 | coding、文档同步、专项验证和归档前复核已完成；本次从 `undo` 归档到 `do` |
| 目标版本 | V2，分支 `mem_ut_uvm_v2` |
| DUT 端口 | `io_ooo_to_mem_isStoreException` |
| 主要 owner | `lsqcommit_agent_agent`、`lsq_commit_handler` |
| 计划边界 | 只适配字段归属和异常 sideband 驱动，不改测试框架主控制流 |

## 1. 目标与术语

### 1.1 目标

当前 `io_ooo_to_mem_isStoreException` 错挂在 `vecissue_agent_agent`，并且实际固定驱动为 0，导致
store fault 仍按 load exception address 来源处理。本 plan 只完成以下闭环：

```text
fault UID 的 load/store 类型
  -> lsq_commit_handler 生成 isStoreException
  -> lsqcommit agent 驱动 DUT
  -> 无新 transaction 时保持最近值
```

不改变 normal commit、LSQ 出队、redirect/replay、pass/fail 或 terminal 主体逻辑。

### 1.2 专有名词

| 术语 | 含义 | 本 plan 中的落点 |
|---|---|---|
| `agent owner` | 唯一声明、驱动、连接和采样某个 DUT 端口的 agent | `lsqcommit_agent_agent` |
| `fault head` | 当前 modeled ROB head 对应的 fault terminal candidate | `has_fault_head/fault_uid` |
| `level sideband` | 没有独立 valid、气泡期间必须保持的输入 | `isStoreException` 和 driver cache |
| `latch` | 最近一次已发送 fault 的异常类型保存值 | `latched_is_store_exception` |
| `normal commit` | 已完成且无 fault 的 ROB commit batch | `commit_uids`/`mark_rob_commit_batch()` |

### 1.3 关键函数职责

- `fault_uid_is_store_exception()`：只根据主表 UID 派生 ROB exception commit type 的 store bit。
- `build_lsqcommit_xaction()`：选择 normal batch 或 fault head，并生成本拍 sideband。
- `mark_fault_rob_commit_uid()`：transaction 发送后更新 fault 类型 latch。
- `lsqcommit_agent_agent_driver::send_pkt()`：原样驱动 transaction，不重新判断操作类型。
- `drive_active_idle()`：无 item 时保持 level sideband，只清 pulse 字段。

## 2. 当前问题与修改原因

| 当前问题 | 修改原因 | 修改后行为 |
|---|---|---|
| 字段位于 vector issue interface/xaction/driver/monitor/connect | `isStoreException` 来自 ROB exception context，不属于 `issueVldu` | 删除 vecissue 链路，唯一迁移到 lsqcommit 链路 |
| vecissue driver 固定驱动 0 | load fault 碰巧正确，store fault 永远选择错误的 LQ 来源 | fault head 按 ROB commit type 驱动 0/1 |
| 不能把 `isStoreException` 等同于 `pendingst` | store fault 不属于 normal pending store；普通 store 也不一定是 exception | 只有 fault head 更新该字段，normal store 不更新 |
| xaction 若使用无约束 `rand bit` 会被旧通用 sequence 随机化 | 旧 testcase 不应因新增字段改变行为 | 字段使用非 `rand bit`，构造默认 0，仅 handler/directive 显式赋值 |
| driver 气泡可能把最近的 store exception 清成 0 | DUT 对应 ROB 值是 level 保存语义 | driver cache 在 no-item/pre-gap/post-gap 期间保持最近值 |
| 非 `MEMBLOCK_UT` 是完整 core readback，ROB latch 没有 reset valid | 不能对没有 valid 锚点的 RTL 值强制 X/Z 检查 | standalone driver-owned 模式检查 X/Z，非 UT 只采样不检查 |

## 3. DUT 语义与目标行为

V2 RTL 将该字段送入 LSQ exception address 选择路径：

```text
io_ooo_to_mem_isStoreException
  -> LSQWrapper exceptionAddr.isStore
  -> 下一拍选择 StoreQueue 或 LoadQueue exception address 来源
```

它只选择基础异常地址来源，不产生 fault，也不直接推进 commit/deq/terminal；atomic、misalign、
vsegment 等已有更高优先级 override 保持不变。

V2 ROB 的异常 commit type 由 `RegEnable(..., exceptionHappen)` 保存，测试框架按同一最小语义建模：

| 场景 | `pendingst` | `isStoreException` |
|---|---:|---:|
| normal scalar store | 1 | 保持上一次 fault 类型 |
| scalar store fault/CBO fault | 0 | 1 |
| scalar load fault | 0 | 0 |
| normal load | 0 | 保持上一次 fault 类型 |
| atomic fault | 0 | 0；最终地址仍由 atomic override 处理 |

## 4. 修改方案

### 4.1 Agent ownership 和字段链路

#### 删除 `vecissue_agent_agent` 中的字段

修改以下文件，删除字段声明、modport 项、xaction field 注册、driver 固定赋值、monitor 局部采样和
两个 connect 分支中的 force/readback：

```text
mem_ut/ver/ut/memblock/agent/vecissue_agent_agent/src/vecissue_agent_agent_interface.sv
mem_ut/ver/ut/memblock/agent/vecissue_agent_agent/src/vecissue_agent_agent_xaction.sv
mem_ut/ver/ut/memblock/agent/vecissue_agent_agent/src/vecissue_agent_agent_driver.sv
mem_ut/ver/ut/memblock/agent/vecissue_agent_agent/src/vecissue_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/tb/vecissue_agent_connect.sv
```

#### 加入 `lsqcommit_agent_agent` 链路

修改对应 interface、xaction、driver、monitor 和 connect：

```text
xaction -> driver::send_pkt() -> drv_cb -> lsqcommit_agent_connect -> DUT input
DUT readback -> mon_cb -> lsqcommit monitor local sample
```

要求：

1. xaction 新增 `bit io_ooo_to_mem_isStoreException`，不是 `rand`，构造默认 0；保留
   `uvm_field_int`、`psdisplay()` 和手工 `compare()` 覆盖。
2. interface 的 `drv_cb` 方向为 output，`mon_cb` 方向为 input。
3. `MEMBLOCK_UT` 下只能由 lsqcommit connect force 该 DUT input；非 `MEMBLOCK_UT` 保留 RTL readback。
4. monitor 不发布新的 analysis transaction、raw queue 或 status，字段只用于链路观察。
5. `dut_inst.sv` 和 generated `MemBlock.sv` 已有该 DUT 端口，不新增或改名顶层端口。

### 4.2 Handler 生成与 latch

新增 handler 私有字段：

```systemverilog
bit latched_is_store_exception;
```

生命周期：

```text
new/reset_lsqcommit_runtime_state: latch = 0
clear_lsqcommit_xaction: tr.isStoreException = latch
normal commit/普通 idle: 不修改 latch
fault head:
  tr.isStoreException = fault_uid_is_store_exception(fault_uid)
  finish_item 成功后 mark_fault_rob_commit_uid 复用同一 helper 更新 latch
redirect/deq/terminal: 不单独清除 latch
```

`fault_uid_is_store_exception()` 的实现：

```text
检查 UID 和主表 transaction 有效；
调用既有 operation behavior 派生逻辑；
返回与 V2 ROB commit type 一致的 commit_is_store；
非法/unsupported 组合沿用既有 uvm_fatal，不默认当作 load。
```

`build_lsqcommit_xaction()` 只在 `has_fault_head` 时覆盖本拍字段；不得提前更新 latch。这样可以
保证“先驱动 DUT，后提交 handler 状态”，与现有 `send_lsqcommit_cycle()` 顺序一致。

### 4.3 Driver level 保持

在 `lsqcommit_agent_agent_driver` 新增：

```systemverilog
bit cached_is_store_exception;
```

行为：

```text
new/reset: cache 清 0，cached_sideband_valid 清无效
send_pkt(tr): 驱动 tr.isStoreException，并更新 cache
drive_active_idle(): 保持 cache；只清 scommit/flushSb
DRV_0 idle: cache 有效则保持，否则驱动 0
其他 drv_mode: 按既有 1/X/random/0 风格覆盖该字段
```

driver 不读取主表、不调用 operation behavior、不修改 status；它只负责传输和保持。

### 4.4 现有 fault smoke 的最小适配

不新增 testcase、virtual sequence 或独立 probe。扩展现有：

```text
mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_fault_smoke_sequence.sv
```

复用现有两 UID directed 主表，分别验证：

1. load fault head 生成 `isStoreException=0`。
2. store fault head（STA 或 STD target）生成 `isStoreException=1`，且 `pendingst/pendingMMIOld/scommit` 仍为 0。
3. fault terminal/idle transaction 保持最近类型；case reset 后恢复 0。
4. 每个 case 重新 reset 主表、LSQ model 和 handler 私有状态；两个 case 完成后再统一调用
   `data.end_test_check()`，避免提前关闭公共 monitor capture。

该 smoke 只验证框架生成的字段和状态闭环，不新增 DUT exception address checker，也不改变原有
writeback、commit、deq 和 terminal 实现。STA IQ-hit/strict STA 继续使用既有 writeback smoke 合同，
本 plan 不新增或修改该合同。

## 5. 明确不修改的逻辑

- 不修改 `pendingPtr`、`pendingst`、`pendingMMIOld`、`scommit` 的生成规则。
- 不修改 `select_rob_commit_batch()`、`mark_rob_commit_batch()`、fault token、LQ/SQ deq、redirect/replay。
- 不把 `isStoreException` 加入 `has_progress`、global stop、pass/fail 或 terminal 判断。
- 不修改 exceptionVec、DUT 最终异常地址 checker、RM、scoreboard、coverage 或 monitor analysis 输出。
- 本轮只支持 scalar LS fault；vector LS/`issueVldu` 仍按现有 unsupported 边界处理。
- standalone reset 驱动 0 只是验证环境安全初值，不代表完整 core 的 ROB `RegEnable` 有 RTL reset 值。
- 不新增 driver 白盒 probe、strict STA 专项、plusarg、cfg 字段、raw queue 或 virtual sequence。

## 6. Coding 顺序与文档同步

按以下顺序 coding，避免 DUT input 短暂无 owner 或双 force：

1. 同一提交中删除 vecissue 字段链并加入 lsqcommit interface/xaction/connect。
2. 修改 handler 的 latch、helper 和 xaction 生成顺序。
3. 修改 lsqcommit driver 的驱动和 level 保持。
4. 扩展现有 fault smoke，完成字段级 load/store 检查。

主要实现文件：

```text
mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv
mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_fault_smoke_sequence.sv
```

sequence 侧只修改现有 sequence 文件，不新增 package/filelist 项；仍需按常规检查 `seq_pkg.sv`/`seq.f` 中无需增加文件。

同步检查并更新：

```text
AI_DOC/mem_ut_flow_doc/rob_commit_lq_sq_deq_flow.md
AI_DOC/mem_ut_flow_doc/fault_exception_flow.md
AI_DOC/analysis/interface/v2/mem_ut_v2_agent_interface_signal_matrix_20260709.md
AI_DOC/analysis/source_sv/dispatch_framework_sv/lsq_commit_handler.md
AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_lsqcommit_dispatch_sequence.md
AI_DOC/analysis/source_sv/dispatch_framework_sv/soft_test_memblock_dispatch_smoke_sequence.md
```

文档只需同步 owner、字段生成时机、level 保持和“不改变 commit/deq/terminal”的边界；不为本字段
新建 RM、coverage 或独立 monitor 输出文档。

## 7. 快速验收

人工 review 和 coding 完成后确认：

```text
1. 有效源码中只有 lsqcommit connect 负责 MEMBLOCK_UT force；vecissue 不再出现该字段。
2. xaction 默认 0，旧通用随机 sequence 不会随机改变该字段。
3. load fault=0、store fault=1；normal store 不被当成 exception。
4. no-item/pre-gap/post-gap 保持最近值，reset 后为 0。
5. `MEMBLOCK_UT` force 分支和非 UT readback 分支均可编译，V2 scalar smoke 无 UVM_ERROR/FATAL。
6. commit/deq/redirect/replay/pass/fail/terminal 行为与原框架一致。
```

## 8. 与原测试框架逻辑对比和修改类型总结

### 8.1 字段适配

原逻辑：字段挂在 vecissue，xaction 虽声明但 driver 固定清零，lsqcommit 没有该字段。

新逻辑：字段从 vecissue 全链路删除，加入 lsqcommit 的 interface/xaction/driver/monitor/connect；
非 `rand` 默认 0，补齐 debug/compare 和双模式 connect。

### 8.2 功能逻辑新增

原逻辑：所有 fault 都把 `isStoreException` 当作 0，store fault 错选 LQ exception address；气泡也会
由 vecissue idle 清零。

新逻辑：fault UID 经统一 helper 得到 load/store 类型；fault transaction 驱动 0/1，发送后 latch，
driver 在无 item 期间保持。该新增功能只影响异常地址来源选择，不改变 fault 产生、commit、deq 或终态。

### 8.3 兼容逻辑与不变项

原逻辑中 normal commit、`pendingst`/`scommit`、LQ/SQ mapping 和 terminal owner 的职责保持；
本 plan 只增加一个异常 sideband 的字段来源和驱动保持，不新增公共状态机、参数、队列或调度拓扑。

## 9. 执行中补充/修正（IMPLEMENTATION_DELTA）

### 9.1 fault smoke helper 改名

`[IMPLEMENTATION_DELTA]`

- 来源：VCS elaboration 检查发现派生类task不能以不同参数列表覆盖父类同名virtual task。
- 原 plan：只要求扩展现有fault smoke，未约束其专项helper名称。
- 实现调整：将fault smoke子类的两参数helper命名为`commit_and_deq_fault_lsq()`；父类原有无参数
  `commit_and_deq_lsq()`保持不变。
- 原因：两者分别承担fault token收敛和normal pass收敛，参数列表不同；保留同名会导致VCS报
  `Invalid number of args to class function commit_and_deq_lsq`，无法完成编译。
- 影响范围：仅
  `soft_test_memblock_dispatch_fault_smoke_sequence.sv`及其源码分析文档的调用名称；不改变任何ROB commit、
  LQ/SQ deq、pass/fail、terminal或DUT驱动逻辑。

### 9.2 software-only smoke 输出 monitor 隔离

`[IMPLEMENTATION_DELTA]`

- 来源：专项仿真启动后，`io_mem_to_ooo_int_wb_agent_agent_monitor`在 software-only testcase 尚未启动
  fault sequence 前读取未驱动的 `writebackStd_0_valid=X` 并无条件 `uvm_fatal`。
- 原 plan：fault smoke只直接调用公共 helper 和 synthetic writeback event；未显式约束不消费的DUT output monitor。
- 实现调整：`tc_smoke`在向环境下发 cfg 前调用一个默认空的`configure_smoke_env_cfg()`钩子；
  `soft_test_tc_dispatch_smoke`覆写该钩子，将现有ctrl、int writeback、vec writeback、wakeup、IQ feedback
  agent的`mon_sw`设为`OFF`。`memblock_env::connect_phase()`同时只在对应`mon_sw=ON`时连接这五个
  `mon_item_port`，避免关闭monitor后解引用未创建的port。
- 原因：这些monitor的raw queue不是software smoke的输入，保留它们只会让未驱动DUT output的X值在软件状态
  闭环之前终止testcase。关闭monitor比修改X/Z检查或伪造DUT output更小、更符合software-only边界。
- 影响范围：只影响`soft_test_tc_dispatch_smoke`及其子类的环境创建，以及`mon_sw=OFF`时这五个既有
  monitor端口的连接保护；不新增cfg字段，不改变monitor源码、raw queue、默认`mon_sw=ON`的real smoke、
  DUT接口、commit/deq、pass/fail或terminal逻辑。

### 9.3 store fault smoke 复用既有 STA IQ-hit 前置合同

`[IMPLEMENTATION_DELTA]`

- 来源：本轮`tc_dispatch_fault_smoke`仿真显示，software-only构造直接向STA item注入real/fault
  writeback时，既有`writeback_status_handler`按V2严格模式报`WB_STATUS_STA_ORDER`，因为缺少当前issue的
  IQ-hit事件。
- 原 plan：只要求fault smoke覆盖STA store fault，没有展开该testcase如何满足已存在的STA writeback前置条件。
- 实现调整：fault smoke在每个synthetic STA real/fault writeback前，复用replay smoke已有raw IQ feedback
  入口，推入同一`uid/sq key/issue instance`的hit事件并调用`collect_monitor_event_batch()`；load-fault case的
  年轻normal UID固定为load，不再无关地触发第二个STA normal-writeback场景。
- 原因：这是满足既有strict STA合同的最小test构造，不修改`writeback_status_handler`、IQ feedback adapter、
  `sta_real_wb_pass_en`或真实STA writeback flow；本专项仍只检查`isStoreException`的fault类型与保持语义。
- 影响范围：仅`soft_test_memblock_dispatch_fault_smoke_sequence.sv`及其说明文档；store fault仍通过STA target
  产生`isStoreException=1`，normal commit/deq/pass/fail/terminal的owner不变。

### 9.4 fault smoke 复用既有 recovery drain

`[IMPLEMENTATION_DELTA]`

- 来源：STA前置条件修正后的专项仿真到达`end_test_check()`，报告runtime drain未完成；根因是
  `mark_target_fault()`已把fault event写入`exception_event_q`，而旧fault smoke只构造commit/deq，没有消费
  该recovery队列。
- 原 plan：说明fault smoke注入fault并收敛commit/deq，但没有显式写出synthetic fault event的recovery消费步骤。
- 实现调整：每轮`inject_fault_writeback_events()`之后调用既有`exception_redirect_replay_task()`，由
  `exception_redirect_replay_handler::process_pending_events()`消费fault event；该handler不重复落fault状态。
- 原因：复用主flow的唯一recovery owner，避免soft smoke直接删除`exception_event_q`或伪造runtime drain完成。
- 影响范围：仅fault smoke的测试构造和说明文档；不修改fault判定、status写入、ROB commit、LQ/SQ deq、
  pass/fail、terminal或global stop逻辑。

## 10. 完成与归档结论（2026-08-06）

本专项对应的源码、flow/analysis 文档和 implementation review 均已完成。归档前复核确认：

- `vecissue_agent_agent` 及其 connect 中没有 `isStoreException` 残留；`lsqcommit_agent_agent` 是该 DUT input
  的唯一 interface/xaction/driver/monitor/connect owner。
- `fault_uid_is_store_exception()` 只从 fault UID 的权威 operation behavior 派生类型；normal
  `pendingst/pendingMMIOld/scommit` 不会覆盖该 fault sideband。
- handler 在 fault token 成功建立后才更新 `latched_is_store_exception`；driver 的 active idle/no-item/pre-gap/
  post-gap 路径保持缓存值，reset 后安全驱动 0。
- 既有 software fault smoke 已覆盖 load fault 为 0、scalar store fault 为 1、fault waiting/terminal 保持和
  case reset；STA 场景复用既有 IQ-hit 前置与 recovery event owner。
- 历史专项验证和 real smoke 验证记录见关联 implementation review 第 9 节；本次仅完成归档性文档复核，
  未重新修改功能源码或重复运行仿真。

本 agent 归档前复核未发现 P0-P3 blocker，结论为 `FINAL PASS`。本 plan 的边界保持不变：它不实现
DUT exception address checker、RM/scoreboard、vector LS 或完整 core ROB readback/reset 建模。
