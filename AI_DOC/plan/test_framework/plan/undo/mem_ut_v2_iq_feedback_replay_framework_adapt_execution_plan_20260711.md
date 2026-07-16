# mem_ut V2 IQ Feedback Replay 适配最终 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | `undo`，待 coding |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| V2 接口权威 | `build_memblock/rtl/MemBlock.sv` |
| 测试框架入口 | `memblock_dispatch_base_sequence::collect_monitor_event_batch()` |
| 适配原则 | 保持 monitor raw 保真、使用现有 current status 作为 generation 权威、按采样 cycle 处理 raw，不新增历史 token/tombstone 系统 |
| 创建/修订日期 | 2026-07-15 |

## 1. 范围与边界

本 plan 只整理 V2 IQ feedback、LDA/STA int-WB raw event 与 STA replay 当前代状态的运行期适配问题。
每个问题均说明 V2 问题、修改原因、最终修改逻辑和可直接 coding 的文字伪代码。

本轮支持范围：

- scalar STA IQ feedback：V2 顶层真实提供 `valid/hit/sqIdx`。
- scalar LDA/STA int-WB：V2 顶层真实提供完整 ROB key 和对应 metadata。
- replay 后 gen1 的 LDA/STA/IQ event 通过现有 status 中的 `issue_epoch/replay_seq` 补齐 current snapshot。
- 同一 service 中积压的 IQ、int-WB、ctrl raw 按 `raw.cycle` 分组处理。
- STA 当前 generation 只允许 `IQ miss -> replay` 或 `IQ hit -> real-WB` 两条路径。

本轮不支持：

- vector IQ feedback、`writebackVldu`、vector 部分 replay 或多 uop 并行 generation。
- STD IQ feedback；STD int-WB value-only 反查由 int-WB 专项负责。
- 新增 issue-generation token、closed tombstone、pending/seen before-image、claim map 或历史匹配系统。
- RM、scoreboard、checker 和 coverage 实现。

本 plan 不修改 `issue_queue_scheduler::mark_issue_fire()` 和
`mark_issue_fire_already_accepted()` 的 generation 建账流程。accepted fire 已经把当前
`issue_epoch` 写入 `status_transaction`，`replay_seq` 也已经由 status 维护；本轮只把这些 current
字段传播到 V2 raw event 生成的 semantic event。

## 2. 问题一：V2 raw key 被伪造成不存在的 ROB/LQ/SQ

### V2 问题

V2 `staIqFeedback_0/1.feedbackSlow` 只带 `valid`、`hit` 和完整 SQ key。当前 monitor 却可能把
`rob_valid/lq_valid` 置为 1，且 value 来自 empty raw 的 0，adapter 后续会把 ROB0/LQ0 当成真实
身份字段。

LDA/STA int-WB 同样只提供完整 ROB key，不提供 LQ/SQ key。若 monitor 把无来源的 LQ/SQ valid
置 1，会污染 UID 解析和 required key 检查。

### 修改原因

V2 raw 必须表达 DUT 端口事实。不存在的 key 不能在 monitor 里补造，否则 adapter 会用伪 key
解析 UID，轻则 drop 正确 event，重则命中错误 transaction。

### 修改方案与修改逻辑

monitor 只写真实存在的物理 key：

| event | raw 中的真实 key | raw 中必须无效的 key |
|---|---|---|
| scalar STA IQ feedback | SQ | ROB、LQ |
| LDA int-WB | ROB | LQ |
| STA int-WB | ROB | SQ |
| STD int-WB | int-WB 专项的 value-only 表示 | 不进入本 plan 的 current snapshot helper |

`writebackLda_0` 是 LoadUnit/AtomicsUnit 混合物理端口。普通 scalar LOAD 可以进入本 plan 的
current snapshot 适配；若真实 ROB owner 的 `main_tr.op_class == MEMBLOCK_OP_CLASS_AMO`，必须在
LOAD/LQ 检查前以 `INT_WB_UNSUPPORTED_ATOMIC` fail-fast。

当前受支持 producer 域中 LDA `replayInst=1` 不可达。converter 必须在 owner 查询、current snapshot
attach、normalize 和 pass/fault 状态更新前以 `INT_WB_REPLAY_INST_UNREACHABLE` fail-fast。

### 文字伪代码

```text
STA IQ monitor 采样：
  如果 lane valid=0，保持 empty raw；
  如果 lane valid=1：
    raw.valid=1；
    raw.source=STA_IQ；
    raw.hit=真实 hit；
    raw.sq_valid=1，并复制真实 sqIdx flag/value；
    raw.rob_valid=0；
    raw.lq_valid=0；
    raw.cycle=当前采样 cycle；
    push 到 raw_iq_feedback_q；
  monitor 不解析 UID，不读取 status，不附加 generation。

LDA/STA int-WB monitor 采样：
  如果 LDA/STA lane valid=0，保持 empty raw；
  如果 lane valid=1：
    raw.valid=1；
    raw.source_kind=LDA 或 STA；
    raw.rob_valid=1，并复制真实 robIdx flag/value；
    对 LDA 保持 lq_valid=0；
    对 STA 保持 sq_valid=0；
    保存 exception、replayInst 和 int-WB 专项允许的 metadata；
    raw.cycle=当前采样 cycle；
    push 到 raw_int_wb_q；
  monitor 不补造 LQ/SQ key。

int-WB converter：
  raw.replay_inst=1 时，立即以 INT_WB_REPLAY_INST_UNREACHABLE fatal；
  对 LDA/STA 构造只带真实 ROB 的 partial event；
  调用 attach_current_issue_snapshot()；
  attach 内部如果解析到 LOAD_WB + AMO owner，立即以 INT_WB_UNSUPPORTED_ATOMIC fatal；
  attach 成功后才允许设置 real_wb_valid 或 fault event 字段。
```

## 3. 问题二：replay 后 event 缺少 current issue snapshot

### V2 问题

V2 STA IQ feedback 和 LDA/STA int-WB 不携带测试框架的 `issue_epoch/replay_seq`。第一次 replay 后，
`normalize_feedback_event()` 会拒绝缺少这两个 snapshot 的非 STD event，导致 gen1 真实 event
被 warning/drop。

### 修改原因

accepted fire 已经在 status 中保存 current `issue_epoch`，replay 流程已经在 status 中维护
`replay_seq`。V2 适配不需要新增历史 token，只需要在 raw event 根据真实 key 唯一命中 current
动态实例后，把 status 中的 current snapshot 附加到 event。

### 修改方案与修改逻辑

新增 `dispatch_monitor_event_adapter::attach_current_issue_snapshot(ref memblock_wb_event_t wb_event)`。

该 helper 只在以下 event 上使用：

- `source=STA_FEEDBACK,target=STA` 的 scalar STA IQ event，用真实 SQ key 解析 UID。
- `source=LOAD_WB,target=LOAD` 的 LDA normal/fault event，用真实 ROB key 解析 UID。
- `source=STORE_WB,target=STA` 的 STA normal/fault event，用真实 ROB key 解析 UID。

helper 复用现有 active ROB/LQ/SQ associative map，禁止每 event 全表扫描。只有 UID、target、
active lifecycle、required key owner 和 current generation 都证明一致后，才写入
`uid/canonical key/issue_epoch/replay_seq` 及 has 位。任一证明失败都说明 valid DUT event 与测试框架
生命周期不一致，固定 fatal。

### 文字伪代码

```text
attach_current_issue_snapshot(event)：
  检查 event.valid=1，且 target 是 LOAD 或 STA；
  根据 source/target 检查 partial event 的真实 key 形态：
    STA IQ 必须只有 SQ；
    LDA/STA int-WB 必须只有 ROB；
    出现伪 ROB/LQ/SQ valid 时 fatal；

  调用现有 active key map 解析 UID：
    STA IQ 用 SQ key 查 active SQ owner；
    LDA/STA int-WB 用 ROB key 查 active ROB owner；
    0 命中或多义命中均 fatal；

  读取 main_tr；
  如果 event.source=LOAD_WB 且 main_tr.op_class=AMO：
    以 INT_WB_UNSUPPORTED_ATOMIC fatal；

  读取 status；
  要求 status.active=1；
  要求 target 已 dispatched；
  要求 status 未 killed、未 flushed、未 redirect pending；
  如果 status.replay_pending 已覆盖当前 target，则 fatal；
  要求 target issue_epoch 非 0；

  核对 required key：
    LOAD 要求 active_lq_mapped=1，status canonical LQ 的 active owner 仍是同一 UID；
    STA 要求 active_sq_mapped=1，status canonical SQ 的 active owner 仍是同一 UID；
    ROB key 必须等于 status canonical ROB；

  写 event.uid、canonical ROB/LQ/SQ；
  写 event.issue_epoch=status 当前 target issue_epoch；
  写 event.replay_seq=status.replay_seq；
  置 has_uid/has_issue_epoch/has_replay_seq 和 required key has 位；
  函数只修改 event，不更新 status、queue、map 或 handler 状态。
```

## 4. 问题三：normalize 缺少 current snapshot 强制合同

### V2 问题

旧逻辑允许部分 event 在 snapshot 缺失时从 current status fallback 补字段。对 V2 STA IQ 和
LDA/STA WB 来说，fallback 会掩盖 adapter 漏附 snapshot 的错误，也可能在 replay 后把旧 event
包装成当前 generation。

### 修改原因

V2 current event 的身份必须在 normalize 之前完成证明。normalize 只能检查已经附加的 snapshot
与 key 一致，不应再猜测 generation。

### 修改方案与修改逻辑

新增 `common_data_transaction::event_requires_current_issue_snapshot()`，用 event 现有
`source/target/action` 定义需要 snapshot 的范围。`normalize_feedback_event()` 对命中该谓词的 event
强制要求 `has_uid/has_issue_epoch/has_replay_seq` 完整。

该谓词不新增 `adapter_origin` 字段，也不依赖 synthetic/real 身份。任何 producer 只要构造出
STA IQ、LDA WB 或 STA WB 语义，就必须主动提供完整 current snapshot。

### 文字伪代码

```text
event_requires_current_issue_snapshot(event)：
  如果 source=STA_FEEDBACK：
    返回 target=STA 且 iq_feedback_valid=1；
  如果 source=LOAD_WB：
    返回 target=LOAD 且 real_wb_valid=1 或 event 带 fault；
  如果 source=STORE_WB：
    返回 target=STA 且 real_wb_valid=1 或 event 带 fault；
  其它 source 返回 0。

normalize_feedback_event(event)：
  先执行现有 action、UID 和 key 归一化；
  如果 event_requires_current_issue_snapshot(event)=1：
    要求 has_uid=1；
    要求 has_issue_epoch=1；
    要求 has_replay_seq=1；
    任一缺失以 CURRENT_SNAPSHOT_MISSING fatal；
    禁止从 current status fallback 补 generation；
  否则：
    redirect、memoryViolation、exception-info、STD 和旧兼容事件保持原 fallback；
  返回 normalized event。
```

## 5. 问题四：raw queue 积压时不同 cycle 被混成一个 batch

### V2 问题

旧 `collect_monitor_event_batch()` 会把当前可见的 int-WB、IQ 和 ctrl raw 一次性清空到同一个
semantic batch。若 raw queue 积压多个 cycle，更晚 cycle 的 redirect 可能覆盖更早 cycle 的 event，
或更早 cycle 的 deq 被延后到更晚 event 之后才应用。

### 修改原因

STA IQ/WB 顺序和 active map 生命周期都依赖采样时间。不同 cycle 必须独立执行
semantic conversion、redirect-first、handler、deferred ctrl apply 和 recovery。

### 修改方案与修改逻辑

`collect_monitor_event_batch()` 先冻结本次 service 可见的三类 raw 到局部 queue，再循环选择三个
queue 头部最小 `cycle`。每个 cycle 形成一个 semantic batch；同 cycle 内固定先转换 IQ，再转换
int-WB，最后转换 `memoryViolation` redirect。当前 cycle 的 semantic batch 处理完成后，按原 raw
顺序应用本 cycle ctrl/deq，再调用一次 replay/redirect recovery，之后才进入下一 cycle。

该流程只比较三个局部 queue 的头部，复杂度为 O(N)，不扫描主表。

### 文字伪代码

```text
collect_monitor_event_batch()：
  把本次 service 可见的 raw_iq_feedback_q 冻结到 local_iq_q；
  把本次 service 可见的 raw_int_wb_q 冻结到 local_wb_q；
  把本次 service 可见的 raw_ctrl_q 冻结到 local_ctrl_q；
  processed_cycle_group=0；

  while 三个局部 queue 至少一个非空：
    next_cycle = 三个非空 queue 头部 cycle 的最小值；
    清 iq_group、wb_group、ctrl_group、events；

    pop local_iq_q 中 cycle==next_cycle 的 raw 到 iq_group；
    pop local_wb_q 中 cycle==next_cycle 的 raw 到 wb_group；
    pop local_ctrl_q 中 cycle==next_cycle 的 raw 到 ctrl_group；

    按采样顺序转换 iq_group：
      convert_raw_iq_feedback()；
      成功 event 追加到 events；

    按采样顺序转换 wb_group：
      convert_raw_int_wb()；
      成功 event 追加到 events；

    遍历 ctrl_group：
      memoryViolation 有效时转换为 redirect event；
      redirect event 不完整时 fatal；
      成功 redirect 追加到 events；

    调用 process_monitor_event_batch(events)：
      对当前 cycle 做 normalize、redirect-first、allowed event check/handler/commit；

    按 ctrl_group 原 raw 顺序调用 apply_raw_ctrl_deq(raw)：
      count=0 raw 也必须调用，以更新 sb_is_empty；

    调用 exception_redirect_replay_task()：
      让本 cycle 产生的 replay/redirect 在下一 cycle 转换前生效；

    processed_cycle_group=1；

  如果 processed_cycle_group=0：
    仍调用一次 exception_redirect_replay_task()；
  service_monitor_once() 返回后不得再额外调用第二次 recovery。
```

## 6. 问题五：STA 当前代返回阶段没有单向状态约束

### V2 问题

V2 scalar STA miss 在 S1 产生 IQ miss feedback 后被 kill，不会进入后级形成同 generation 的
real-WB。IQ hit 才能进入后级并随后产生 real-WB。旧方案把 `IQ/WB` 任意先后、miss 后旧 WB 迟到
当作可接受路径，不符合 V2 StoreUnit 时序。

### 修改原因

`issue_epoch/replay_seq` 只回答 event 属于哪次发射；还需要轻量状态位回答本次 STA 发射已经走到
哪个返回阶段。否则无法区分 WB-before-IQ、miss-after-WB、重复 IQ 或重复 WB。

### 修改方案与修改逻辑

`status_transaction` 新增两个字段：

| 字段 | 含义 | 清零点 | 置位点 |
|---|---|---|---|
| `sta_iq_feedback_seen` | 当前 STA generation 已接受过 IQ feedback | reset、STA 新 fire、STA replay、redirect 清理 | IQ handler 成功后 |
| `sta_real_wb_seen` | 当前 STA generation 已接受过真实 STA WB | reset、STA 新 fire、STA replay、redirect 清理 | STA real-WB handler 成功或唯一 compat no-op 后 |

新增两个 helper：

- `check_current_issue_event_stage(event)`：handler 前只读检查 current snapshot 和 STA 阶段。
- `commit_current_issue_event_stage(event)`：handler 成功后写 seen 位。

`handle_issue_feedback_event()` 中 IQ hit 必须总是先记录 `sta_issue_feedback_success=1`，再根据
`MEMBLOCK_STA_REAL_WB_PASS_EN` 决定等待 real-WB 或走兼容 pass。

### 文字伪代码

```text
STA accepted fire：
  sta_iq_feedback_seen=0；
  sta_issue_feedback_success=0；
  sta_real_wb_seen=0；
  状态进入 WAIT_IQ。

check_current_issue_event_stage(event)：
  如果 event 不需要 current snapshot，直接返回；
  重新读取 status；
  要求 event.issue_epoch/replay_seq 等于 status current 值；
  要求 status 仍 active、target dispatched、未 kill/redirect/flush/replay；

  如果 event 是 STA IQ：
    如果 sta_iq_feedback_seen=1，以 STA_DUPLICATE_IQ fatal；

  如果 event 是 STA real-WB 或 STA fault-WB：
    如果 sta_iq_feedback_seen=0，以 STA_WB_BEFORE_IQ fatal；
    如果 sta_issue_feedback_success=0，以 STA_WB_AFTER_IQ_MISS fatal；
    如果 sta_real_wb_seen=1，以 STA_DUPLICATE_WB fatal；
    如果 MEMBLOCK_STA_REAL_WB_PASS_EN=0 且 event 不是 normal WB：
      以 STA_COMPAT_FAULT_WB fatal；
  helper 不修改 status。

process_allowed_non_redirect_event(event)：
  对 current event 先调用 check_current_issue_event_stage(event)；
  调用原 IQ 或 real-WB handler；
  如果 handler 成功：
    调用 commit_current_issue_event_stage(event)；
    返回成功；
  如果 handler 失败但满足唯一 compat no-op：
    event 是 STA normal real-WB；
    MEMBLOCK_STA_REAL_WB_PASS_EN=0；
    IQ hit 已由兼容路径标记 pass；
    event 无 fault、无 replay；
    sta_real_wb_seen 仍为 0；
    调用 commit_current_issue_event_stage(event)，只记录真实 WB 已到达；
    返回成功；
  其它 current event handler reject 均 fatal。

commit_current_issue_event_stage(event)：
  再次核对 event generation 仍等于 current status；
  如果 event 是 STA IQ：
    要求 sta_iq_feedback_seen=0；
    置 sta_iq_feedback_seen=1；
  如果 event 是 STA real-WB 或 fault-WB：
    要求 sta_iq_feedback_seen=1 且 sta_issue_feedback_success=1；
    要求 sta_real_wb_seen=0；
    置 sta_real_wb_seen=1；
  LOAD event 不修改 STA 阶段位。
```

## 7. 问题六：负向场景不能用裸 make 返回码判定 expected fatal

### V2 问题

STA 顺序错误、缺 snapshot、不可达 `replayInst` 和 unsupported atomic 都需要 expected-fatal 验证。
裸 `make` 非零返回可能来自编译失败、远端失败、timeout 或其它 fatal，不能作为目标 fatal 命中的证明。

### 修改原因

每个负向用例只应验证一个 fatal 原因，且必须确认目标 fatal ID 唯一命中。否则可能把环境失败误判成
专项逻辑正确。

### 修改方案与修改逻辑

新增 `sim/run_expected_fatal.sh`，固定以 `basicTest + ts=<vseq>` 运行一个 negative vseq，读取确定性
log，并要求目标 fatal ID 恰好出现一次、`UVM_FATAL=1`、`UVM_ERROR=0`，同时拒绝编译/链接错误、
远端 SSH 错误、timeout、`TEST_PASS` 和 `TEST CASE PASSED`。

negative vseq 不新增 testcase。除 missing snapshot 直接构造 semantic event 外，其余场景必须通过
真实 raw queue、adapter 和 batch timeline 进入框架，不得直接调用 handler 绕过本 plan 路径。

### 文字伪代码

```text
run_expected_fatal.sh <vseq_class> <fatal_id> [cfg_name]：
  检查 vseq_class 和 fatal_id 非空；
  cfg 未传时使用 default；
  固定 tc=basicTest、mode=base_fun、timing=rtl、seed=666666；
  根据 sim 日志命名规则计算唯一 log 路径；
  删除同名旧 log；

  调用 make eda_batch_run 并保存返回码；
  如果 log 不存在：
    脚本失败，报告 simulation failed without result log；

  要求 fatal_id report tag 恰好出现一次；
  要求 UVM 汇总为 UVM_ERROR=0、UVM_FATAL=1；
  拒绝其它 UVM_FATAL report tag；
  拒绝 compile/link/ssh/timeout/TEST_PASS/TEST CASE PASSED 字样；
  全部满足时打印 EXPECTED_FATAL_PASSED 并返回 0；
  返回码本身只作为诊断，不作为唯一通过条件。
```

## 8. Coding 落点汇总

| 文件 | 对应问题与修改 |
|---|---|
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_iq_feedback_agent_agent/src/io_mem_to_ooo_iq_feedback_agent_agent_monitor.sv` | 问题一：STA IQ raw 改为 SQ-only |
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_monitor.sv` | 问题一：LDA/STA raw 只保留真实 ROB key；STD 由 int-WB 专项处理 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv` | 问题二、四：current snapshot attach；raw cycle timeline 转换与 ctrl 延后应用 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv` | 问题三、五：current snapshot 谓词、normalize 强制合同、STA stage helper |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/status_transaction.sv` | 问题五：新增 STA 当前代阶段位 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_batch_handler.sv` | 问题五：allowed event 进入原 handler 前后执行 stage check/commit |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/writeback_status_handler.sv` | 问题五：IQ hit 始终记录 feedback success |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv` | 问题四：按 cycle group 处理 raw；删除 service 返回后的第二次 recovery |
| `mem_ut/ver/ut/memblock/seq/virtual_sequence/soft_test` | 问题六：正向和 expected-fatal vseq |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/soft_test` | 问题六：复用 negative soft sequence |
| `mem_ut/ver/ut/memblock/seq/seq_pkg.sv`、`seq/seq.f` | 问题六：接入新增 soft sequence/vseq |
| `mem_ut/ver/ut/memblock/seq/plus_cfg/memblock_dispatch_replay_sta_compat.cfg` | 问题五、六：compat 正向场景显式关闭 `MEMBLOCK_STA_REAL_WB_PASS_EN` |
| `mem_ut/ver/ut/memblock/sim/run_expected_fatal.sh` | 问题六：expected-fatal 日志判定脚本 |

明确不修改：

```text
issue_queue_scheduler::mark_issue_fire()
issue_queue_scheduler::mark_issue_fire_already_accepted()
LSQ enqueue sequence/driver
ROB commit/deq owner
RM、scoreboard、checker、coverage
任何 vector LS 主流程
```

## 9. 修改类型与原逻辑对比总结

| 修改项 | 类型 | 修改前逻辑 | 修改原因 | 修改后逻辑 |
|---|---|---|---|---|
| STA IQ raw key | raw 事实修正 | monitor 可能伪造 ROB/LQ valid，value 为 0 | V2 顶层没有这些字段 | 只保留真实 SQ key，ROB/LQ 无效 |
| LDA/STA WB raw key | raw 事实修正 | monitor 可能伪造 LQ/SQ valid | V2 split WB 只提供完整 ROB | LDA/STA 只保留 ROB；required LQ/SQ 从 current status 核对后补 canonical key |
| generation 来源 | 功能逻辑修改 | 旧 plan 引入 token/tombstone 历史系统 | accepted fire 已保存 current `issue_epoch/replay_seq` | 用真实 key 解析 current UID 后从 status 附加 snapshot |
| normalize 合同 | 功能逻辑修改 | snapshot 缺失时存在 fallback | replay 后 fallback 会猜错 generation | current event 必须在 normalize 前已带 UID、issue_epoch、replay_seq |
| raw batch 编排 | 功能逻辑修改 | 多个 cycle raw 混入一个 batch | redirect/deq 可能跨 cycle 错序 | 按最小 cycle 分组，IQ -> WB -> ctrl semantic -> handler -> ctrl apply -> recovery |
| STA 阶段 | 状态生命周期修改 | IQ/WB 可任意先后，miss 后 WB 可能被当作迟到 | 不符合 V2 StoreUnit 时序 | `WAIT_IQ -> miss replay` 或 `WAIT_IQ -> hit WAIT_WB -> WB`，非法顺序 fail-fast |
| compatibility 模式 | 行为边界收紧 | IQ hit 兼容 pass 后真实 WB 处理不清 | 需要记录真实 WB 到达且不掩盖 fault | 只允许后到 normal WB no-op；后到 fault WB fatal |
| expected-fatal 验证 | 验证策略修改 | 可能用裸 make 非零作为通过 | 非零可能来自环境失败 | 脚本检查目标 fatal ID 唯一命中和 UVM 汇总 |

保持不变的主体逻辑：issue fire 建账、原 IQ hit/miss handler、原 real-WB pass/fault handler、
redirect-first 仲裁、exception/replay recovery、terminal/pass/fail 定义。
