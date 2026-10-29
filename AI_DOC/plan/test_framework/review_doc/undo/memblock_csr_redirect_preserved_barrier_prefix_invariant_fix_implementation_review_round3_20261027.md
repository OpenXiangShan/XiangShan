# MemBlock CSR Redirect Preserved Barrier Prefix 实现审查 Round 3

**日期**：2026-10-27
**Reviewer**：独立 implementation reviewer
**结论**：`REVIEW_PASS`
**工作树**：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025`
**审查方式**：只读源码、plan、既有日志与结果文件；未修改生产源码，未启动新的编译或仿真。
**范围限制**：只审查 CSR redirect preserved barrier prefix/control-barrier 问题；明确不审查 stale TLB/RM root、residency、issue snapshot 相关修改。

## 1. Findings 与术语说明

### 1.1 Findings-first 结论

未发现 blocking finding。Round 2 的两项阻塞均已关闭：

1. **P0 已关闭**：control terminal retire 已拆成完整只读 precheck 和不再调用 report-capable helper 的直接 commit；正常 `terminal_count=0` 且 active ROB map 缺失的 caught-fatal case 证明失败发生在 status、frontier、map、queue、tombstone、retry、barrier、owner/generation 和 audit count mutation 之前。
2. **P1 已关闭**：SFence、L2 ASSERT、L2 RELEASE 的 enqueue/start/completion 均通过真实生产入口触发 duplicate lifecycle fatal；统一快照覆盖四类 queue 的完整顺序、ROB/LQ/SQ maps、flushSb live 状态、runtime snapshot/observation、owner/generation/state/action 和全部 audit counts。
3. **L2 ASSERT baseline 顺序已修正**：baseline 只在 lifecycle precheck 成功后写入，不再出现 duplicate fatal 已被 catcher 捕获但 baseline 提前改变的副作用。
4. 未发现 checker 放宽、DUT/RTL/Scala/Chisel 修改、未预期 fatal 被吞、或以直接调用底层 precheck 伪造生产入口 PASS。

### 1.2 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `lifecycle event` | 一个 control marker 生命周期中的唯一事件，例如 enqueue、start、completion、terminal 或 barrier release | `memblock_control_audit_event_e`、`control_audit_*_count` | 同一 SFence enqueue 第二次发生时必须在任何 queue mutation 前 fatal |
| `precheck` | 只读取状态、验证事件合法性并生成提交票据的阶段 | `precheck_control_lifecycle_event()`、`precheck_control_terminal_retire()` | ROB map 不一致时返回失败，不能先清 retry 或 queue |
| `no-fatal commit` | precheck 全部成功后执行的直接状态提交段；不再调用可能 fatal 的 lookup/helper | `commit_control_terminal_retire()`、`commit_csr_control_action()` | 删除 active ROB map并置 terminal 后不再出现可捕获 fatal |
| `audit ticket` | precheck 成功后冻结本次 UID、事件类型和 generation 的提交凭据 | `memblock_control_lifecycle_ticket_t` | mutation 成功后由 `record_control_lifecycle_event()` 把对应 count 从 0 置 1 |
| `active map` | 用物理 ROB/LQ/SQ key 反查当前 active UID 的关联表 | `uid_by_active_rob`、`uid_by_lq`、`uid_by_sq` | terminal retire 必须存在唯一且精确的 active ROB owner，且不能残留 LQ/SQ owner |
| `atomic snapshot` | duplicate 负向测试调用生产入口前保存的完整可变状态集合 | `memblock_control_atomic_snapshot` | caught fatal 后比较 queue 完整顺序、maps、owner、observations 和 audit counts 全部不变 |
| `production entry` | 真实运行流程调用的 enqueue/pop/service/terminal/release 函数，而不是测试直接调用底层审计 helper | `enqueue_sfence_action()`、`try_pop_*()`、`service_*()`、`try_retire_control_committed_uid()` | duplicate completion 通过 service 入口触发，而不是直接调用 `precheck_control_lifecycle_event()` |
| `sentinel` | 为证明 queue 没有 pop、替换或重排而预置的可区分元素 | directed 中地址、snapshot seq 或 observation seq 不同的两个元素 | SFence start失败后 `0x111`、`0x222` 两个 action 顺序必须保持 |

## 2. 审查范围与依据

### 2.1 对应 Plan 与前轮 Review

- Plan：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/AI_DOC/plan/test_framework/plan/undo/memblock_csr_redirect_preserved_barrier_prefix_invariant_fix_plan_20261025.md`
- Round 2 review：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/AI_DOC/plan/test_framework/review_doc/undo/memblock_csr_redirect_preserved_barrier_prefix_invariant_fix_implementation_review_round2_20261027.md`
- 文档规则：`AI_DOC/project_management/mem_ut_code_review_document_rule.md`、`AI_DOC/project_management/ai_doc_language_rule.md`

### 2.2 本轮重点源码

- `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_control_barrier_service.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_rob_admission_window_sequence.sv`

### 2.3 验证证据

- `mem_ut/ver/ut/memblock/sim/r25_control_directed_20261027/summary.log`
- `mem_ut/ver/ut/memblock/sim/r25_control_directed_20261027/results.tsv`
- `mem_ut/ver/ut/memblock/sim/r25_control_directed_20261027/status/seed_2601277.final.log`
- `mem_ut/ver/ut/memblock/sim/csr_prefix_fix_compile_20261027_r25/log/tc=basicTest_ts=memblock_rob_admission_window_directed_vseq_cfg=default_seed=2601277_rtl_eda_reg_20261027_023106_2235918_3502.log`

## 3. P0：Terminal Retire 原子性

### 3.1 修改前逻辑与 Round 2 风险

修改前的 control terminal 路径在 lifecycle precheck 后先写 `success/terminal_done`，再调用通用 `retire_active_uid()`。通用 retire 会在清 retry、issue/PTW 和 tombstone 之后检查 ROB/LQ/SQ map；若 map 检查 fatal 被 catcher 捕获，测试流程仍会继续，但状态已经部分提交。

### 3.2 只读 retire precheck

抽象功能描述：`precheck_control_terminal_retire()` 由 control terminal 入口在任何 retire mutation 前调用；它验证将被删除的 active ROB、LQ/SQ owner、issue/PTW work、late tombstone 和 terminal frontier，并只计算计划提交的 frontier，不负责写 terminal 状态或删除容器。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:7237-7356`，函数 `precheck_control_terminal_retire()`。

```systemverilog
if (!uid_by_active_rob.exists(rob_map_key) ||
    uid_by_active_rob[rob_map_key] != uid) begin
    `uvm_fatal("CONTROL_COMMIT", ...)
    return 1'b0;
end
...
planned_terminal_done_uid = scan_uid;
return 1'b1;
```

中文伪代码：该函数先确认目标 UID active、尚未 terminal 且没有 LQ/SQ 映射标志；再验证精确 ROB map、全表中没有第二个 ROB owner、LQ/SQ map 中没有残留 owner，并扫描 load/STA/STD issue queue、PTW replay queue 和 STA/STD tombstone。任一不一致只报告 fatal并返回失败；全部通过后只读扫描 terminal 前缀，输出计划的新 frontier，不修改任何生产状态。

正确性检查：该 precheck 覆盖 Round 2 指出的 fatal-capable 通用 retire 检查，并把 frontier 空洞/null status 也纳入 mutation 前检查。函数体没有 queue/map/status 写入。

### 3.3 No-fatal commit

抽象功能描述：`commit_control_terminal_retire()` 只在上述 precheck 成功后执行；它直接清理当前 UID 的 retry/work/tombstone，删除已验证的 active ROB map，写 success、terminal、frontier 和 active 状态，不负责重新校验身份或调用通用 retire helper。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:7361-7408`，函数 `commit_control_terminal_retire()`。

```systemverilog
uid_by_active_rob.delete(rob_map_key);
status.success = 1'b1;
status.terminal_done = 1'b1;
dispatch_progress.terminal_done_uid = planned_terminal_done_uid;
status.active = 1'b0;
```

中文伪代码：commit 使用 precheck 已确认的 UID 与 frontier，直接清零 uncache retry 字段，删除该 UID 的 issue/PTW 项和 tombstone，再删除目标 ROB map并一次性写入成功、终态、frontier 和 inactive。该段不调用 `get_status()`、map lookup、通用 retire 或其他可能 fatal 的 helper；唯一附加动作是 `uvm_info` 日志。

正确性检查：commit 段不存在第二个可捕获失败点；`record_control_lifecycle_event()` 在 commit 完成后才记录 terminal count。

### 3.4 Caller chain

抽象功能描述：`try_retire_control_committed_uid()` 是 control ROB commit 的生产入口；它先完成 barrier、redirect、provenance 和 lifecycle event 检查，再执行 retire precheck，全部成功后才进入 commit和审计记录。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:7577-7621`，函数 `try_retire_control_committed_uid()`。

```systemverilog
if (!precheck_control_lifecycle_event(uid, MEMBLOCK_CONTROL_AUDIT_TERMINAL,
                                      audit_ticket)) return;
if (!precheck_control_terminal_retire(uid, planned_terminal_done_uid)) return;
commit_control_terminal_retire(uid, planned_terminal_done_uid);
record_control_lifecycle_event(audit_ticket);
```

中文伪代码：入口先生成 terminal audit ticket；ticket 不合法立即返回。然后执行完整 retire precheck；ROB/map/work/frontier 任一失败立即返回。只有两个 precheck 都成功，才直接提交 retire 状态，最后把 terminal audit count 置 1。失败路径不会进入 commit，也不会记录一次未完成的 terminal 事件。

### 3.5 ROB map caught-fatal Directed

抽象功能描述：`run_control_lifecycle_duplicate_checks()` 中的 ROB-map case 用正常 `terminal_count=0` 和完整 action 矩阵通过 lifecycle precheck，再故意删除当前 UID 的 active ROB map，调用真实 terminal 生产入口并验证捕获 fatal 后所有目标状态保持不变。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_rob_admission_window_sequence.sv:862-936`。

```systemverilog
data.uid_by_active_rob.delete(rob_map_key);
...
data.try_retire_control_committed_uid(uid);
...
if (catcher.caught_count != 1 ||
    status.control_state != state_before || status.active != active_before ||
    status.terminal_done != terminal_before || status.success ||
    data.dispatch_progress.terminal_done_uid != terminal_frontier_before || ...)
    `uvm_fatal(...)
```

中文伪代码：测试先建立可进入 terminal 的真实 control status，将 retry字段设为非零并删除 active ROB map；随后保存 status、terminal frontier、ROB/LQ/SQ map 数量、三类 issue queue、PTW queue、STA/STD tombstone、retry、barrier、owner/generation 和所有 audit count。调用真实 `try_retire_control_committed_uid()` 后要求只捕获一个 `CONTROL_COMMIT` fatal，并逐项比较所有已保存状态不变。

结论：该 case真实跨过 lifecycle precheck，失败点位于新的 retire precheck；因此不是旧 duplicate-terminal case那种提前被 `terminal_count=1` 挡住的伪覆盖。

## 4. P1：完整 Duplicate Lifecycle 矩阵

### 4.1 统一原子快照

抽象功能描述：`memblock_control_atomic_snapshot` 是 software-only duplicate case 的测试对象；它在调用生产入口前复制所有可能由该入口改变的 queue、map、owner-visible status、runtime observation 和 lifecycle counter，并在 fatal 被 catcher 捕获后做完整相等比较。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_rob_admission_window_sequence.sv:35-285`，class `memblock_control_atomic_snapshot`。

```systemverilog
csr_q = data.csr_control_action_q;
sfence_q = data.sfence_control_action_q;
release_q = data.l2_flush_release_request_q;
flushsb_q = data.flushsb_req_q;
foreach (data.uid_by_active_rob[key]) uid_by_active_rob[key] = data.uid_by_active_rob[key];
...
foreach (csr_q[idx]) if (csr_q[idx] != data.csr_control_action_q[idx]) return 1'b0;
```

中文伪代码：capture 阶段完整复制四类 queue，不只保存 size/front；逐 key复制 ROB/LQ/SQ maps，并保存 SFence C0/effective observation、flushSb attached/active/completed、terminal frontier、barrier、owner/generation/state/action、runtime CSR、expected payload、L2 baseline、各 baseline sequence、package observation 和全部 audit counts。compare 阶段先验证 queue大小和每个元素顺序，再验证 map key/value集合，最后比较所有标量和结构体字段。

正确性检查：该 helper直接关闭 Round 2 “仅比较单元素 CSR queue和少数字段”的缺口。测试 setup 用到但生产 duplicate入口不写的 topology开关不属于 mutation target；测试在结束时恢复这些开关。

### 4.2 SFence enqueue/start/completion

抽象功能描述：`run_sfence_lifecycle_duplicate_checks()` 建立合法 L2TLB topology 和 SFence owner状态，分别调用真实 service enqueue、真实 queue pop和真实 completion service，验证 duplicate fatal 前后统一快照相等。

源码位置：`soft_test_memblock_rob_admission_window_sequence.sv:1011-1178`。

```systemverilog
service.enqueue_sfence_action(status);
...
if (data.try_pop_sfence_control_action(popped_action)) `uvm_fatal(...)
...
service.service_sfence_control(status);
```

中文伪代码：enqueue case预置两个地址不同的 SFence sentinel并把 enqueue count设为1，通过 `enqueue_sfence_action()` 进入真实 topology检查和 `enqueue_sfence_control_action()`；start case通过真实 `try_pop_sfence_control_action()` 验证 peek/precheck先于pop；completion case预置匹配的 C0/effective observation，通过 `service_sfence_control()` 进入真实 completion precheck，证明 duplicate失败时 observation不被消费、state不推进。

生产代码正确性：`service_sfence_control()` 先用 `peek_control_sfence_effective_observation()`确认完成事实，再做 lifecycle precheck；只有成功后才调用消费 helper并进入 `CONTROL_COMMIT_READY`。duplicate count已为1时在消费 observation前返回。

### 4.3 L2 ASSERT 与 L2 RELEASE

抽象功能描述：`run_check_store_lifecycle_duplicate_checks()` 建立 check-store runtime baseline和 done observation，覆盖 ASSERT enqueue/start/completion以及 RELEASE enqueue/start/completion的真实生产入口。

源码位置：`soft_test_memblock_rob_admission_window_sequence.sv:1180-1403`。

```systemverilog
service.enqueue_check_store_l2_assert(status);
if (data.try_pop_csr_control_action(popped_assert)) `uvm_fatal(...)
service.service_check_store_control(status);
...
if (data.try_pop_l2_flush_release_request(popped_release)) `uvm_fatal(...)
service.service_check_store_control(status);
```

中文伪代码：ASSERT enqueue通过真实 service读取 runtime CSR baseline与done-low observation；ASSERT start通过共享 CSR queue consumer并用两个 snapshot sequence不同的sentinel证明完整顺序不变。done-high进入 `service_check_store_control()` 后，先分别测试 ASSERT completion duplicate和同一原子事务中的 RELEASE enqueue duplicate。RELEASE start使用两个 observation baseline不同的request证明不pop、不重排；done-low再次通过真实 service验证 completion duplicate不推进state、不改 observation和审计计数。

正确性检查：六个 case 都只捕获 `CONTROL_LIFECYCLE_AUDIT`，每个 `caught_count` 必须精确为1，并使用同一个完整 snapshot helper。测试没有直接调用底层 precheck代替生产入口。

### 4.4 L2 ASSERT baseline 写入顺序

抽象功能描述：`enqueue_check_store_l2_assert()` 在 runtime facts齐备时构造 ASSERT action；它必须先完成 lifecycle precheck，再冻结 status中的 L2 CSR baseline并提交 action。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_control_barrier_service.sv:594-650`。

```systemverilog
if (!data.precheck_control_lifecycle_event(status.uid,
        common_data_transaction::MEMBLOCK_CONTROL_AUDIT_L2_ASSERT_ENQUEUE,
        audit_ticket)) return;
status.control_l2_csr_baseline_valid = 1'b1;
status.control_l2_csr_baseline = snapshot;
data.commit_csr_control_action(action, audit_ticket);
```

中文伪代码：函数先读取当前 done-low、control runtime baseline和最新 CSR snapshot并构造本地 action；随后执行 duplicate/predecessor precheck。precheck失败立即返回，status baseline保持原值。只有成功时才写 baseline并调用只做 queue/status/event mutation的 `commit_csr_control_action()`。

结论：Round 2之后要求的 baseline “移到 precheck后”已经落实，directed的完整快照也包含 baseline valid/payload，可检测回归。

## 5. 生产调用链与原子性判断

| 流程 | 生产入口 | Precheck | Commit/消费 | 审计记录 | 原子性结论 |
|---|---|---|---|---|---|
| terminal | `try_retire_control_committed_uid()` | lifecycle + retire全量只读precheck | `commit_control_terminal_retire()` | terminal count | PASS；commit无fatal helper |
| SFence enqueue | `enqueue_sfence_action()` | topology + lifecycle | `sfence_control_action_q.push_back()` | SFence enqueue count | PASS；duplicate在push前返回 |
| SFence start | `try_pop_sfence_control_action()` | peek front + lifecycle | `pop_front()` | SFence start count | PASS；双元素顺序有directed证明 |
| SFence completion | `service_sfence_control()` | peek observation + lifecycle | consume observation + state推进 | SFence completion count | PASS；duplicate不消费observation |
| L2 ASSERT enqueue | `enqueue_check_store_l2_assert()` | runtime facts + lifecycle | baseline写入 + CSR queue commit | ASSERT enqueue count | PASS；baseline已在precheck后 |
| L2 ASSERT start | `try_pop_csr_control_action()` | profile识别 + lifecycle | `pop_front()` | ASSERT start count | PASS；和CSR start按profile分开 |
| ASSERT completion + RELEASE enqueue | `service_check_store_control()` | 两张ticket统一precheck + request只读构造 | record ASSERT + commit RELEASE | 两个独立count | PASS；commit段无fatal lookup |
| L2 RELEASE start | `try_pop_l2_flush_release_request()` | peek front + lifecycle | `pop_front()` | RELEASE start count | PASS；双元素顺序有directed证明 |
| L2 RELEASE completion | `service_check_store_control()` | done-low + lifecycle | state推进 | RELEASE completion count | PASS；duplicate在state写入前返回 |

## 6. Checker、RTL 与测试真实性审计

### 6.1 Checker 是否放宽

未发现放宽。`precheck_control_lifecycle_event()`仍要求count为0和严格前序矩阵；duplicate fatal通过参数化 catcher按精确 report ID捕获，其他 fatal继续 `THROW`。测试要求每个 case `caught_count==1`，最终 UVM summary仍要求 `UVM_ERROR=0/UVM_FATAL=0`。

### 6.2 DUT/RTL 是否修改

本 control-only review范围内没有 DUT RTL、Scala/Chisel 或 DUT接口逻辑修改。当前工作树中的其他 SystemVerilog/RM修改属于 stale TLB/RM专项或其他共享工作，不纳入本结论。

### 6.3 测试是否伪造

未发现以直接调用 audit precheck伪造PASS。software-only case确实人工建立合法 owner、topology和monitor observation，但失败触发点全部经过真实 enqueue/pop/service/terminal/release生产入口。人工事实只用于把生产入口推进到目标duplicate边界；完整快照保证这些事实本身不会被失败路径消费或重写。

19个 expected fatal与源码矩阵一致：原有boundary/redirect负向case、CSR/control lifecycle、terminal ROB-map、SFence三阶段和check-store六阶段均被执行。日志因 catcher返回 `CAUGHT` 不打印每条fatal正文，但每个case内部均要求自己的局部 `caught_count==1`；任何case未命中都会由测试自身产生未捕获 fatal，不能靠总数掩盖缺项。

## 7. r25 验证证据

- `summary.log:1-16`：`result=PASS`、planned/completed/settled=`1/1/1`、pass=`1`、fail/infra/incomplete/aborted=`0`。
- `results.tsv:2`：seed `2601277`，`PASS`，`EXIT_CONFIRMED`，`rc=0`，`uvm_count=0`。
- `status/seed_2601277.final.log:2025`：出现 `TEST_PASS`。
- 同日志 `2044`：caught `UVM_FATAL=19`；`2055-2056`：最终 `UVM_ERROR=0`、`UVM_FATAL=0`。
- 同日志 `2086-2087`：正常 `$finish`，仿真时间 `295.100ns`。
- `status/compile.final.log` 末尾完成174 modules、partition stitch和link，生成并确认 `csr_prefix_fix_compile_20261027_r25/exec/simv`。
- 未发现 threshold kill、timeout、incomplete 或非零返回码。

## 8. Plan 对齐检查

### 8.1 对齐结论

本轮重点实现符合 plan第5.7.1、5.7.2和第7章验收要求：

- 所有目标 lifecycle event均为 mutation前precheck、成功后record。
- start路径为peek、precheck、pop。
- terminal为完整只读precheck加no-fatal commit。
- duplicate case通过真实生产入口，覆盖完整queue顺序、maps、runtime facts、owner/generation/state/action和全部audit counts。
- L2 ASSERT baseline写入位于precheck之后。

### 8.2 实现与 Plan 不一致项

未发现本轮 P0/P1 实现与 Plan 不一致项；当前 coding行为与对应 plan保持一致。

### 8.3 Plan 未说明但 Coding 落实的细节

Plan已描述ASSERT completion和RELEASE enqueue必须组成可预检的原子事务。当前实现使用两张 `audit_ticket`加 `precheck_l2_assert_completion_release_enqueue()`，属于该要求的直接落地，不构成额外设计偏离。本轮未发现需要回写 plan 的新增 control-only语义。

## 9. 残余风险与非阻塞验证缺口

1. r25是software-only directed，证明的是 caught-fatal 原子性，不替代真实worker/driver/monitor并发调度、原始失败seeds、fresh随机矩阵和邻接回归；这些仍应按主plan最终门禁完成或引用已冻结的同版本证据。
2. terminal负向动态证据直接注入的是active ROB map缺失；duplicate terminal和release也有生产入口覆盖。LQ/SQ残留、issue/PTW残留、tombstone和frontier null等其他precheck分支本轮主要依赖静态只读审计，没有逐分支独立directed注入。
3. P0 directed已覆盖Round 2要求的全部关键状态，但没有把status对象的每个无关字段做整对象序列化比较；本轮通过源码确认失败发生在纯读取precheck中，因此不构成原子性blocker。
4. SFence/check-store tests直接发布合法package observation以到达completion边界；它们不验证monitor是否能在真实时序生成这些observation，只验证生产service在duplicate失败时不消费、不推进状态。

上述项目不影响本轮针对Round 2 P0/P1 blocker的关闭结论，但不得将r25单项directed扩张解释为主plan全部回归已经完成。

## 10. 非本次修改的逻辑分析

### 10.1 git status 对比结论

本次review主题覆盖的逻辑文件为：

- `common_data_transaction.sv`中的control lifecycle、terminal retire和相关queue helper。
- `memblock_control_barrier_service.sv`中的CSR/SFence/check-store lifecycle service。
- `soft_test_memblock_rob_admission_window_sequence.sv`中的control-only directed负向矩阵。
- 指定的plan、Round 2 review和r25结果目录。

`git status --short`中存在但不属于本次review主题的修改：

| 类别 | 文件/目录 | 判断 | 原因 |
|---|---|---|---|
| stale TLB/RM专项源码 | `memblock_rm.sv`、`memblock_rm_readonly_api.sv`、`memblock_tlb_entry.sv`、`memblock_pma_pmp_model.sv`、issue driver/sequence及相关soft tests | 另行review | 属于root/residency/issue snapshot、PMA/PMP generation等独立RM方案，用户明确排除 |
| redirect及其他测试框架源码 | `exception_redirect_replay_handler.sv`、`issue_queue_scheduler.sv`、`status_transaction.sv`等 | 仅审查与本control调用链直接相关片段 | 文件包含共享工作树中的其他逻辑，不能整体归入本结论 |
| RM plan/review文档 | `AI_DOC/plan/rm_plan/undo/*`、`AI_DOC/plan/rm_review_doc/undo/*`、RM buglist | 另行review | 对应stale TLB/RM独立方案 |
| 仿真/seed产物 | `AI_DOC/seed/`、其他simulation结果 | 非源码review | 不属于本轮指定r25证据集合 |

## 11. Verdict

`REVIEW_PASS`

Round 2的P0 terminal no-fatal commit和P1完整duplicate生产矩阵均已关闭；当前control-only实现可以通过本轮独立implementation review。残余风险仅限第9章所列验证广度，不构成代码blocker。
