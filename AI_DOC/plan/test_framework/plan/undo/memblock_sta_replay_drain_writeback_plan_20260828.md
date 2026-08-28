# MemBlock STA replay-drain 写回生命周期修复方案（2026-08-28）

| 项目 | 内容 |
| --- | --- |
| 状态 | 已停止执行；二次回归发现纯 replay 不产生 raw STA 的合法路径，已由 late-fault tombstone plan 替代 |
| 版本 | V2，`mem_ut_uvm_v2` |
| 关联分析 | `AI_DOC/analysis/framework_design/memblock_sta_replay_late_writeback_rm_issue_analysis_20260828.md` |
| 修改范围 | `status_transaction`、`common_data_transaction`、`writeback_status_handler`；不改 RTL、DUT interface、cfg 或 PMA/PMP 模型 |
| 验证目标 | Sv39/U 态 real-dispatch 10000 笔回归继续运行，消除 replay 后迟到 STA raw 的误报 |

> 替代关系：本 plan 的“等待 raw 再应用 replay”行为会使 UID9 一类纯 replay
> 永久保留 `sta_dispatched`，因此不再作为 coding 依据。后续实现只以
> `AI_DOC/plan/test_framework/plan/do/memblock_sta_replay_late_fault_tombstone_coding_plan_20260828.md`
> 为准；本文件保留用于说明 UID25 的首轮证据和被替代的假设。

## 1. 专有名词与抽象功能说明

| 英文术语 | 当前中文含义 | 代码落点 | 示例 |
| --- | --- | --- |
| `replay drain` | STA 已收到 `hit=0`，但旧 pipeline raw 尚未被 RM 消费的受控过渡状态。 | `status_transaction.sta_replay_drain_pending` | UID25 的 `520.3ns` 到 `530.3ns`。 |
| `issue snapshot` | 一个真实 STA fire 的 epoch、replay sequence 和 flush epoch 身份。 | `mark_issue_snapshot()` | raw STA 绑定的前置条件。 |
| `current generation` | 当前尚未被 replay/redirect 淘汰的 STA 发射实例。 | `status.sta_dispatched` 与 `sta_issue_epoch` | replay drain 前仍是旧 generation。 |
| `raw writeback` | `writebackSta_*` monitor 采集的实际 DUT 输出。 | `convert_raw_int_wb()` | 可以出现在 IQ replay feedback 之后。 |
| `fault priority` | raw 的 exception 一旦存在，必须按 fault 处理，不能被 replay drain 吞掉。 | `handle_real_writeback_event()` | PF/AF 的 STA raw。 |

抽象功能说明：本 plan 将 STA replay 的“请求重发”和“旧流水线实例已排空”拆成两个阶段。
IQ feedback 只创建 drain 状态；真正清 `sta_dispatched`、递增 replay 序列和开放重发，
只能在对应 raw STA writeback 被消费后发生。该机制不改 DUT 行为，也不把 raw 写回误记为
正常 pass。

## 2. 目标 Flow 与不变量

```text
driver 记录 STA fire
  -> issue scheduler 建立 snapshot，sta_dispatched=1
  -> IQ feedback hit=0
  -> 标记 replay-drain pending，保持 snapshot 与 sta_dispatched
  -> raw STA writeback
  -> 若有异常：fault 优先，清 drain 状态
  -> 若无异常：验证 drain descriptor，消费 raw，调用既有 replay transition
  -> mark_replay_pending 清旧 target，bump replay_seq，route 新 STA candidate
```

不变量：

1. `sta_replay_drain_pending=1` 时，旧 `sta_dispatched` 和 `sta_issue_epoch` 必须保持，
   因而 adapter 可以严格绑定 raw，不新增“无 snapshot 接受”例外。
2. replay drain 不开放新 STA candidate；因此同一 UID/SQ/ROB 不会同时有旧、新两个可被
   raw 模糊匹配的 active generation。
3. `STA IQ hit=1`、Load、STD、redirect 的既有流程不进入本机制。
4. raw fault 优先于 drain；fault 保存完毕后不得再从该 drain 发起 replay。
5. 只读 UID status 与既有 key/epoch，不扫描 `main_trans_num` 或全局队列。

## 3. 数据结构与状态 API

### 3.1 `status_transaction` 的 replay-drain descriptor

抽象功能描述：descriptor 由 IQ feedback failed 创建，由 raw STA 或 fault/flush 消费或
清理。它记录的不是另一套 issue owner，而是当前 `sta_issue_epoch` 的受控 drain 标志。

在 `status_transaction.sv` 新增并在 `reset()` 初始化：

```text
bit          sta_replay_drain_pending;
int unsigned sta_replay_drain_issue_epoch;
int unsigned sta_replay_drain_replay_seq;
```

中文文字伪代码：

```text
创建 status 时三个字段均为零/无效，表示没有 STA replay drain。
IQ feedback hit=0 仅在当前 STA snapshot 仍有效时写入 pending 和其 epoch/replay_seq。
对应 raw 被消费、fault 落表、redirect flush 或 status reset 时清除三个字段。
pending 为 1 时，scheduler 仍看到 sta_dispatched 为 1，因此不会构造重发 candidate。
```

### 3.2 `common_data_transaction::mark_sta_replay_drain_pending()`

抽象功能描述：该函数将一个已归属、已发射的 STA IQ miss 转为“等待旧 raw drain”状态。
它不修改 `sta_dispatched`、不删除 issue snapshot、不递增 `replay_seq`，也不直接入 replay
queue。

计划逻辑：

```text
读取 uid status。
要求 active、STA dispatched、当前 issue_epoch/replay_seq 精确匹配，且没有 fault、redirect、已有 drain。
校验后设置 pending、保存 issue_epoch/replay_seq 和 last_event_cycle。
不调用 mark_replay_pending。
```

中文文字伪代码：

```text
函数由 STA IQ feedback failed 的 writeback handler 调用。
它先确认 feedback 属于仍活动的当前 STA generation；迟到、已 fault、已 redirect 或 epoch
不匹配的反馈返回失败，不污染当前状态。
通过后仅保存当前 generation 的 replay-drain descriptor。因为 dispatched 和 snapshot 仍在，
后续 raw 能继续走严格 adapter 校验。
```

### 3.3 `common_data_transaction::consume_sta_replay_drain_on_raw()`

抽象功能描述：该函数在无异常 raw STA 到达时验证其仍属于 pending 的旧 generation，并将
replay 过渡交给已有 `mark_replay_pending()`。它不把该 raw 计为正常 STA pass。

计划逻辑：

```text
读取 uid status，要求 sta_replay_drain_pending。
比较 raw 附带的 issue_epoch/replay_seq 与 descriptor/current snapshot。
先清 drain descriptor。
调用 mark_replay_pending(uid, STA, issue_epoch, replay_seq, cycle)。
返回 mark_replay_pending 的结果。
```

中文文字伪代码：

```text
函数由真实 raw STA writeback 的 handler 调用，表示旧 StoreUnit 流水线已经可观察地排空。
若 raw 的 snapshot 与 drain descriptor 不一致，立即报 framework fatal，不能把未知 raw 绑定到
任意 replay。
匹配时清 drain 标志，再复用现有 replay transition；该 transition 负责清 STA dispatched、
增加 replay 序列并让 route flow 在后续周期重新构造 STA candidate。
raw 本身不进入 normal-pass 分支，因此不会把重放请求错误记成成功。
```

### 3.4 统一清理

`mark_target_fault()`、`clear_uid_dispatch_result()`、`mark_replay_pending()` 和 status reset
必须清 descriptor。原因是这些路径已经终结或替换旧 generation；保留 drain 会让后续 raw
被错误解释为活跃实例。

## 4. Feedback 与 Raw 处理 Flow

### 4.1 `writeback_status_handler::handle_issue_feedback_event()`

抽象功能描述：该函数处理已由 adapter 绑定 snapshot 的 IQ feedback。STA hit 保持既有
“等待 real writeback”语义；STA hit=0 改为进入 replay drain，而不立即投入 exception/replay
worker。

计划逻辑：

```text
若 target 是 STA 且 iq_feedback_failed：
  调用 mark_sta_replay_drain_pending。
  成功时打印 replay-drain 日志并返回已消费。
  失败时按 stale feedback 记录并返回未消费。
否则：
  保持既有 STA hit、Load/STD 的处理逻辑。
```

中文文字伪代码：

```text
该分支只拦截 STA 的 hit=0。它不再把 replay event 放入 exception_event_q，因为 worker 若
立即调用 mark_replay_pending 会过早清除当前 raw 的 owner。
成功创建 drain 后，当前 issue snapshot 仍有效；同一 UID 不会被重新 route，直到 raw 或
fault 消费该状态。
其他 target 和 STA hit=1 完全沿用现有 handler，避免改变正常 issue feedback 的职责。
```

### 4.2 `writeback_status_handler::handle_real_writeback_event()`

抽象功能描述：该函数处理已严格附着 snapshot 的真实 raw 写回。它在 STA normal-pass 前
识别 pending drain，并用 raw 作为 replay 生命周期的完成证据。

计划逻辑：

```text
若 raw 有 exception：
  清匹配的 STA drain（若存在），继续既有 mark_target_fault。
若 target 是 STA 且 pending drain：
  调用 consume_sta_replay_drain_on_raw。
  成功后打印 drain consume 日志并返回，不调用 normal pass。
否则：
  保持既有 IQ-hit 前置检查、fault、normal pass 处理。
```

中文文字伪代码：

```text
真实 fault 拥有最高优先级：它会保留架构异常，而不会被“需要 replay”的早期 feedback 遮蔽。
无异常且存在 drain 时，raw 只证明旧 pipeline 已排空；函数调用 consume helper 触发标准
replay transition 并立即返回，因此不会设置 sta_writeback 或 sta_pass。
不存在 drain 时，原有 real writeback 路径继续要求 STA IQ hit，并按既有规则写 pass/fault。
```

## 5. 失败策略

| 场景 | 策略 |
| --- | --- |
| STA IQ hit=0 对应当前 snapshot | 创建 replay drain，`UVM_INFO`。 |
| hit=0 已 stale/已 redirect/epoch 不匹配 | 不修改状态，按现有 stale 处理。 |
| drain raw 的 issue epoch 或 replay sequence 不匹配 | `uvm_fatal`；说明框架身份不一致。 |
| drain raw 带 exception | fault 优先，清 drain 后走既有 fault。 |
| raw 未到且无 redirect/fault | 保留现有 no-progress watchdog，不新增任意固定等待周期或静默 release。 |
| 无 drain 的普通 STA raw | 保持原 IQ-hit 前置与 normal-pass 流程。 |

## 6. 验证计划

1. `git diff --check`，确认新增核心字段都有 reset、设置、读取和清除点，且有中文注释。
2. `rg` 检查所有 `sta_replay_drain_*` 使用点，确认不会在 Load/STD 路径出现。
3. 远端编译并使用固定 seed、`wave=on` 重跑 `rm_sv39_10k_inflight_fault_20260828` 场景。
4. UID25 验收：日志出现 replay-drain 创建与消费，且不再报 `INT_WB_ATTACH`。
5. 继续同 cfg/seed 的 10000 笔测试；新 RM/framework 失败另建分析和 plan。只有观测到
   没有真实 fire/snapshot 的 DUT raw 时才启动 RTL subagent 复核。

## 7. 文档、review 与提交

- 完成后新增 implementation review 到
  `AI_DOC/plan/test_framework/review_doc/undo/memblock_sta_replay_drain_writeback_implementation_review_20260828.md`。
- review 必须覆盖三个新状态字段、两个 data API、IQ feedback 分支、raw writeback 分支和
  fault/redirect 清理。
- 验证无 blocker 后将本 plan 从 `undo` 移入 `do`，并只提交本 feature 的源码、分析、plan、
  review 和归档移动；不混入用户已有脏改动，不 push。
