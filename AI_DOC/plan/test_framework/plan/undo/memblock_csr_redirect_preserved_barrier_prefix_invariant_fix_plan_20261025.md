# MemBlock CSR Redirect Preserved Barrier Prefix Invariant 修复计划

**文档类型**：V2 测试框架正式可 coding plan
**日期**：2026-10-25（2026-10-26 按第四轮独立 review 修订）
**目标分支**：`codex/memblock-csr-prefix-fix-20261025`
**基线 commit**：`045011bf6bd912410395eeadf01b007a2c11339b`
**关联 Buglist**：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/AI_DOC/buglist/rm/v2/rm_buglist_2026-W43_20261019.md` 的“CSR random redirect 后静态控制屏障被 ROB admission prefix invariant 误报”章节
**当前阶段**：实现、r25 真实编译与 software-only directed 已完成；Round 2 的 terminal 原子性和完整
duplicate 生产入口矩阵均已关闭。独立 implementation review Round 3 于 `2026-10-27` 给出
`REVIEW_PASS`。从单 issue alternate index 导出的 exact tree 又于 `2026-10-28` 完成 r4 强制重编译与
directed gate，19 个预期 `UVM_FATAL` 全部被 catcher 捕获，最终 UVM error/fatal 为零，允许形成单 issue
commit。原始 seed 与邻接回归证据见对应 Buglist，合入母任务后仍需复跑母任务触发的 scalar load seed
`1684208666`，确认其越过相同 prefix 场景。

## 术语表

| 术语 | 当前中文含义 | 代码落点/示例 |
|---|---|---|
| admission prefix | 已按 UID 连续完成 ROB/LSQ admission 的前缀；`get_next_new_admit_uid()` 返回此前缀后的第一个 UID | `common_data_transaction::dispatch_progress.max_enqueued_uid` |
| prefix-external owner | active ROB map 中 UID 不小于当前 `get_active_scan_end_uid()` 的 owner；普通 owner 出现该状态必须 fatal | `assert_active_owners_within_admission_prefix()` 遍历 `uid_by_active_rob` |
| static control barrier | 已 admission、但尚未启动 CSR/SFence/check-store action 的唯一控制标记；不占 LQ/SQ，并阻止年轻 UID admission | `active_control_barrier_uid`，状态 `WAIT_OLDER_ROB_COMMIT` |
| preserve observation | 某次 redirect scan 调用 `preserve_static_control_marker_on_redirect()` 并成功确认 marker 仍为静态 barrier；它只是本次 scan 的局部观察，不等同于 provenance | `apply_redirect_flush_range()` 的局部 `seen + uid` |
| persistent provenance | 与当前 active static barrier 生命周期绑定的“该 marker 曾被真实 rollback 推到 admission prefix 外”证明 | 本 plan 新增 class fields |
| origin rollback | 第一次实际执行 `rollback_max_enqueued_uid()` 并使 marker 从 redirect 前 admission 尾部落到新 prefix 外的事件 | provenance 唯一建立点 |
| consecutive redirect | marker 尚未通过 `restore_control_admission_prefix()` 回到 prefix 前，又发生一次或多次 redirect | provenance 必须保持并允许同一 marker |
| provenance match | 每次 prefix invariant 扫描时，唯一 prefix-external owner 与 persistent provenance、active barrier 和静态 marker 状态完整一致 | assert 的局部 `matched_count` |
| restore | 普通 owner 依次 re-admission 到 marker UID 后，`restore_control_admission_prefix()` 将 prefix 跨回 marker | provenance 唯一正常清除点 |
| started control | 已离开 `WAIT_OLDER_ROB_COMMIT`、已绑定 `control_owner` 或已 enqueue action 的控制标记 | 即使 provenance 存在也必须 fatal |
| negative invariant | 人工构造错误 provenance、普通 prefix-external owner或 started marker，确认 fail-fast 且 catcher 后无继续副作用 | 扩展 ROB admission software-only directed test |

## 1. 问题、责任与首轮 Review

### 1.1 失败场景

`tc_memblock_csr_random_config` 只生成普通 `INT_LOAD`，并按 6--12 个 UID 的间隔插入 CSR control
marker。普通 Load fault 产生 redirect 后，框架允许覆盖范围内尚处于
`WAIT_OLDER_ROB_COMMIT` 的 marker 作为静态 barrier 保留；年轻 Load 被清理并等待重新 admission。

| seed | redirect UID | 保留 marker UID | rollback 后 `new_end_uid` | fatal 时间 |
|---:|---:|---:|---:|---:|
| 2124399471 | 11 | 20 | 12 | 1087.900ns |
| 1596235805 | 26 | 30 | 27 | 2237.600ns |
| 903730789 | 22 | 31 | 23 | 2164.000ns |

三个 marker 均为 `lq/sq_mapped=0/0`。`apply_redirect_flush_range()` 已正确保留静态 marker 并回退
admission prefix，但 `assert_active_owners_within_admission_prefix()` 把 marker 当作普通残留 owner fatal。
seed `1611682665` 由回归 error-limit controller 中止，修复后必须完整重跑。

### 1.2 责任边界

- **测试框架问题**：公共 prefix invariant 未表达静态 control barrier 的 redirect recovery 生命周期。
- **不是 RTL**：DUT fault/redirect 已正常进入框架；无 DUT assertion、协议错误或 X/Z 首发点。
- **不是 RM**：fatal 前 Load compare 正常推进，RM 未产生错误期望。
- **不是普通 owner 清理失败**：被报告 owner 不占 LQ/SQ，且与 active control barrier UID 一致。

### 1.3 首轮独立 review 的四项 blocker

| ID | 首轮问题 | 本次修订 |
|---|---|---|
| B1 | scan-local proof 在 marker 恢复前发生连续 redirect 时丢失，第二次 invariant 无法证明同一合法 marker | 改为绑定 active static barrier 生命周期的 persistent provenance；首次真实 rollback 建立，连续 redirect 保持，restore 成功清除 |
| B2 | 仅扫描到 marker、没有普通 owner flush、没有 rollback 时仍可能建立或要求消费 proof | provenance 只在 `found_flushed` 且 rollback 后 marker 确实位于新 prefix 外时建立；无 rollback 不建立、不匹配、不要求消费 |
| B3 | “同一次 scan 第二个成功 preserve marker”在唯一 active barrier 设计下不可构造；catch fatal 后函数若继续执行会产生副作用 | 删除该生产测试；保留错误 provenance、普通 owner、started marker等可构造负向；所有可被 catcher 捕获的 fatal 分支紧跟 `return` |
| B4 | plan 中没有统一写明 ROB flush 判定的完整调用形式 | 全文统一为 `rob_order_util::rob_need_flush(status.get_rob_key(), redirect)` |

### 1.4 二轮独立 review 的两项 blocker

| ID | 二轮问题 | 本次修订 |
|---|---|---|
| B1 | `preserve_static_control_marker_on_redirect()` 和 provenance establish helper 若被 expected-fatal catcher 吞掉 fatal，void caller仍可能继续记录 observation、执行 invariant和 finalize cancel状态 | 两个 helper统一返回 `bit success`；每个可捕获 fatal分支显式 `return 1'b0`，成功路径 `return 1'b1`；`apply_redirect_flush_range()` 对任一返回0立即 `return`，且 observation只能在 preserve返回1后记录 |
| B2 | software-only ROB admission directed positive A/B/C 越界声称真实 CSR action、terminal和barrier release | A/B/C只验证 rollback、persistent provenance、连续 redirect invariant、restore和clear；真实 worker生命周期由既有 AUTO、MANUAL_CONTROL和 CSR random real-worker cfg验证，不新增 vseq或 cfg |

### 1.5 第三轮独立 review 的两项 blocker

| ID | 第三轮问题 | 本次修订 |
|---|---|---|
| B1 | helper失败只传播到 `apply_redirect_flush_range()`，外层 `apply_redirect_flush()` 仍会继续 PTW replay清理、redirect drive queue删除、flush/active redirect清零和 phase idle | `apply_redirect_flush_range()` 同样改为 `function bit`；任一可捕获 fatal返回0，正常完成全部 cancel finalize返回1。`apply_redirect_flush()` 必须检查返回值，收到0后立即 `return`，严格位于全部外层清理动作之前；software-only directed通过真实 wrapper链证明失败后无这些副作用 |
| B2 | 三个真实 cfg的责任描述不符合实际，且“exactly once”没有可执行字段、断言和日志证据 | 修正为 ROB_CONTROL只覆盖固定 check-store，MANUAL覆盖 CSR/SFence/check-store，CSR random覆盖 redirect后的 CSR。新增 per-marker/per-action lifecycle audit counters、generation约束、release/end-test断言和 `CONTROL_LIFECYCLE_AUDIT` 结构化日志；每个 cfg均按其真实 marker集合核对精确计数和零残留 |

### 1.6 第四轮独立 review 的两项 blocker

| ID | 第四轮问题 | 本次修订 |
|---|---|---|
| B1 | `assert_active_owners_within_admission_prefix()` 和 `audit_active_rob_window()` 仍为 `void`；fatal 被 catcher 捕获后，range 会继续 cancel finalize、deleted-owner window和 aggregate处理。其他 range 内 fatal-capable helper也没有统一列出和传播 | 建立第5.3.2节的完整 catchable-call inventory。所有由 range 直接调用且可能 fatal 的检查/变更 helper统一设计为 `function bit`，或拆成无副作用 `precheck_*()` bit与不再 fatal的 commit；任一0均从 range立即返回，再逐层传播到 wrapper和全部 caller。directed分别在 before-scan audit和 after-scan invariant捕获 fatal，证明 cancel/deleted-owner/PTW/queue/phase均无后续副作用 |
| B2 | lifecycle duplicate检查位于 queue push/pop、generation/owner写入、terminal retire等 mutation之后；catcher吞 fatal时已改变 queue、owner、generation或status | 所有事件改为严格两阶段协议：`precheck_control_lifecycle_event()` 在任何 mutation前验证并返回 ticket；生产 mutation成功后才调用无失败的 `record_control_lifecycle_event(ticket)`。start路径必须先 peek并precheck再pop；owner bind、completion、terminal、release均先precheck。duplicate caught-fatal测试快照并要求 queue/owner/generation/status/count逐项不变 |

## 2. 设计目标与禁止项

### 2.1 必须实现

1. 只有 `assert_active_owners_within_admission_prefix()` 可以对 prefix-external owner 做语义豁免。
2. 豁免必须由 persistent provenance、当前 active barrier 和 marker 静态状态共同证明，不能只看 marker 类型或状态。
3. provenance 只在第一次实际 rollback 将 redirect 前尾部 marker 推到 prefix 外时建立。
4. marker 恢复前的连续 redirect必须保持同一 provenance；每次 invariant 都重新匹配同一个 marker。
5. `restore_control_admission_prefix()` 成功跨回 marker 后立即清除 provenance；不得提前清除。
6. marker terminal、barrier release、testcase reset或最终 end-check 时仍有 provenance，必须 fail-fast；reset 在确认无残留后仍显式清零字段。
7. 仅 marker 被 scan 命中但没有普通 owner flush、没有 rollback时，不建立 provenance，也不要求 invariant 匹配 provenance。
8. 普通 owner、started control、错误 UID、错误 barrier、错误 redirect范围、多个 prefix-external owner继续 fatal。
9. 所有被 expected-fatal catcher 覆盖的 helper fatal 分支必须返回 `1'b0`，禁止 catcher 吞 fatal 后继续修改状态；不得依赖 `void` helper中的 bare `return`。
10. `preserve_static_control_marker_on_redirect()` 和 `establish_control_barrier_prefix_external_provenance()` 必须返回 `bit success`；caller收到0必须立即返回，不得继续产生本轮 redirect副作用。
11. `apply_redirect_flush_range()` 必须返回 `bit success`；第5.3.2节列出的每个 range 内 catchable检查都必须返回bit并被检查。任何前置、before-scan audit、scan helper、rollback、after-scan invariant、cancel-finalize precheck、deleted-owner precheck或aggregate失败均返回0，只有全部步骤成功后才返回1。
12. `apply_redirect_flush()` 必须检查 range返回值；收到0后在 `clear_ptw_wait_replay_by_redirect()`、`clear_redirect_drive_queue()`、redirect phase推进、`flush_in_progress`/`dispatch_flush_in_progress`/`issue_freeze_ack`清零、`active_redirect`清除和 phase idle之前立即返回。
13. 每个真实 control marker必须有可执行 exactly-once lifecycle audit：creation、owner bind/generation、每个真实 action phase的 enqueue/start/completion、terminal和barrier release计数均精确匹配 marker kind，release/end-test时无 live owner、action、queue或 barrier残留。
14. lifecycle audit必须使用“mutation前precheck + mutation成功后record”的两阶段协议。任何 duplicate/非法事件在 queue push/pop、generation/owner、control state、terminal/active或barrier字段改变前返回0；post-record不得成为第二个可捕获失败点。

### 2.2 明确禁止

- 不按 `uid_is_control_marker()`、`WAIT_OLDER_ROB_COMMIT` 或 `lq/sq_mapped=0/0` 单条件泛化豁免。
- 不使用单次 scan-local proof 作为跨 redirect 的合法性依据。
- 不为每次 redirect 重建、覆盖或刷新 origin provenance；连续 redirect只验证并保持原 provenance。
- 不修改 `rollback_max_enqueued_uid()` 的回退算术或成功语义，不保留 redirect 前 `max_enqueued_uid`；仅允许
  将接口改为bit，使UID合法性失败可传播且写prefix前完成。
- 不把 marker 计入 `oldest_flushed_uid`，不取消、重建或递增其 `dynamic_epoch`。
- 不放宽 `preserve_static_control_marker_on_redirect()` 对 started control 的 fatal。
- 不新增“第二个成功 preserve marker”生产测试或依赖不可构造状态证明正确性。
- 不让 software-only ROB admission directed test声明或伪造真实 CSR worker、action enqueue、terminal或barrier release闭环。
- 不新增 vseq或 cfg；专项 software-only case复用现有 ROB admission directed入口，真实 worker验证复用现有三个 cfg入口。
- 不用汇总 PASS 或 `end_test_check()` 的泛化 terminal检查替代 per-marker lifecycle audit；不得只依赖队列最终为空来声称 exactly once。
- 不允许先 push再检查 enqueue、先 pop再检查 start、先递增 generation/写 owner再检查 bind、先更新完成状态再检查 completion、先 retire再检查 terminal或先清 barrier再检查 release。
- 不修改 RM、RTL、Scala/Chisel、DUT 接口、agent connect或 cfg 激励语义。
- 不吞掉 `ROB_ADMISSION_PREFIX`；非法状态仍使用 fatal fail-fast。

## 3. 修改范围

### 3.1 生产测试框架

仅允许修改：

`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`

`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/mem_ut/ver/ut/memblock/seq/base_seq_help/status_transaction.sv`

`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/mem_ut/ver/ut/memblock/seq/base_seq_help/exception_redirect_replay_handler.sv`

允许的逻辑变化：

1. 增加 persistent provenance fields及其初始化、建立、保持、读取、清除和 end-check。
2. `preserve_static_control_marker_on_redirect()` 改为 `bit` 返回；`apply_redirect_flush_range()` 只在返回1后记录本次 preserve observation，返回0时立即退出。
3. provenance establish helper返回 `bit`；`apply_redirect_flush_range()` 在真实 rollback 后决定是否首次建立，返回0时立即退出。
4. `audit_active_rob_window()`、`assert_active_owners_within_admission_prefix()` 以及第5.3.2节全部 range 内 fatal-capable helper均改为 `function bit` 或明确的 bit precheck + non-failing commit；range检查每个返回值并立即传播失败。
5. `restore_control_admission_prefix()` 在成功恢复 prefix 后清除 provenance。
6. control terminal、barrier release、reset和 end-check 对 provenance 残留执行 fail-fast。
7. `apply_redirect_flush_range()` 和 `apply_redirect_flush()` 改为 `function bit`，完整传播失败状态；不得改变成功路径既有清理顺序。
8. 增加严格局部的 per-marker lifecycle audit字段、两阶段 precheck/record helper、release/end-test断言和结构化日志；这些字段只做测试框架审计，不驱动 control状态机。
9. `exception_redirect_replay_handler::advance_active_redirect()` 显式检查 `apply_redirect_flush()` 返回值，0时立即返回；不得增加其他 handler行为变化。

为避免把本修复扩大成共享API重构，第5.3.2节中已有其他生产caller的helper采用**range专用checked入口**：
`apply_redirect_flush_range()` 不再直接调用对应legacy void helper，而调用唯一caller为range的
`*_for_redirect_range_checked()` bit入口。checked入口完成全量只读precheck和无fatal commit；legacy入口及其
无关caller不改签名。audit/invariant/preserve/establish/prepare/rollback等仅由range使用的入口可直接改为bit。
因此每个新增bit入口的全部caller均在本plan中显式列出并检查返回值，不产生共享caller吞错或递归扩散。

`rollback_max_enqueued_uid()` 的回退算术和成功调用语义保持不变；只增加bit返回和mutation前UID precheck。

为在控制动作真正启动前审计 provenance，还允许对以下测试框架文件做一个严格局部的生命周期守卫修改：

`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_control_barrier_service.sv`

允许的变化严格限定为：

1. 在 `service_active_control_barrier()` 的 `MEMBLOCK_CONTROL_STATE_WAIT_OLDER_ROB_COMMIT` 分支中，
   `bind_control_owner(status)` 之前读取 persistent provenance；若仍 valid，则 fatal并立即 `return`。
2. 在既有 owner bind、CSR/SFence/check-store action enqueue和 monitor completion成功点调用 lifecycle audit
   递增 helper。audit不得改变状态机转移、owner token、queue或 control kind行为。

### 3.2 专项测试

只允许扩展既有 software-only sequence：

`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_rob_admission_window_sequence.sv`

以及仅为检查新增 bit返回值而局部修改既有 caller：

`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_pending_mmio_directed_sequence.sv`

由现有 `memblock_rob_admission_window_directed_vseq` 启动，不新增或复制 vseq，不新增 cfg，不改变任何生产
cfg 的随机激励语义。真实 control worker生命周期验证只运行既有
`csr_sfence_check_store_rob_control.cfg`、`csr_sfence_check_store_manual_control.cfg` 和
`tc_memblock_csr_random_config.cfg`。

为覆盖第三轮 B1，software-only sequence还必须从 `data.apply_redirect_flush(redirect)` 真实 wrapper入口注入
一个可由 catcher捕获的 preserve或establish失败，不得只直调 range/helper。调用前后快照至少包含 PTW wait
replay、redirect drive queue/inflight、`redirect_phase`、`flush_in_progress`、
`memblock_sync_pkg::dispatch_flush_in_progress`、`issue_freeze_ack`、`active_redirect`、active cancel record和
cancel finalize/deleted-owner状态；wrapper返回0后逐项相等。

### 3.3 文档同步

coding 后必须检查并同步：

- 本 plan；实现偏离时新增 `IMPLEMENTATION_DELTA`。
- 对应 implementation review 文档。
- `AI_DOC/analysis/framework_design/csr_sfence_rob_control_barrier_flow_draft_20260813.md`。
- `AI_DOC/web/**` 中相关函数、字段、调用关系和状态生命周期。
- W43 buglist 的 review、coding和验证状态。

## 4. Persistent Provenance 数据设计

### 4.1 新增字段

在 `common_data_transaction` 的 active control barrier fields旁新增：

```systemverilog
bit                         control_barrier_prefix_external_provenance_valid;
memblock_uid_t              control_barrier_prefix_external_provenance_uid;
memblock_redirect_payload_t control_barrier_prefix_external_origin_redirect;
memblock_uid_t              control_barrier_prefix_external_origin_old_end_uid;
memblock_uid_t              control_barrier_prefix_external_origin_new_end_uid;
```

字段含义：

- `valid`：当前 active static barrier 已被一次真实 rollback 推到 admission prefix 外。
- `uid`：与 provenance 绑定的 barrier UID，必须始终等于 `active_control_barrier_uid`。
- `origin_redirect`：第一次建立 provenance 的 redirect，仅用于身份审计、日志和 debug；后续 redirect不得覆盖。
- `origin_old_end_uid`：第一次建立前的 scan end；建立时必须满足 `uid == origin_old_end_uid - 1`。
- `origin_new_end_uid`：第一次 rollback 后的 prefix end；建立时必须满足 `uid >= origin_new_end_uid`。

### 4.2 设置规则

provenance 只能由 `apply_redirect_flush_range()` 在以下条件全部成立时首次设置：

```text
found_flushed == 1
已经调用 rollback_max_enqueued_uid(oldest_flushed_uid)
本次 scan 的 preserve observation valid
当前 provenance invalid
preserved_uid == active_control_barrier_uid
preserved_uid == old_end_uid - 1
preserved_uid >= get_active_scan_end_uid()
marker 仍为 active/enq/non-terminal WAIT_OLDER_ROB_COMMIT
marker 无 action owner、无 action enqueue、无 LQ/SQ mapping
rob_order_util::rob_need_flush(status.get_rob_key(), redirect) == 1
```

若只命中 marker但 `found_flushed==0`，不得调用 rollback，不建立 provenance，不要求 assert 匹配。
若 rollback 后 marker仍在 prefix内，也不得建立 provenance。

### 4.3 保持规则

provenance valid期间：

- 后续 redirect不得改写五个 origin字段。
- marker若已在 prefix外，通常不再位于 active scan的 `[begin_uid,end_uid)` 范围，本次 scan可以没有
  preserve observation；合法性由 persistent provenance继续证明。
- 每次 `assert_active_owners_within_admission_prefix()` 都必须重新确认 provenance UID、active barrier、
  marker静态状态和本次 redirect范围一致。
- 后续 redirect即使再次 flush普通 owner并再次 rollback，也只能保持原 provenance。
- 后续 redirect若没有普通 owner flush、不发生 rollback，仍保持已存在 provenance；但不得创建新的
  provenance或更新 origin字段。

### 4.4 正常清除规则

唯一正常清除点是 `restore_control_admission_prefix(uid)`：

1. 先验证 provenance valid、provenance UID与 `uid`/active barrier一致，marker仍为静态等待状态。
2. 调用现有 `mark_uid_enqueued(uid)` 成功跨回 marker。
3. 再将 `valid=0`、UID/origin metadata全部清零。
4. fatal路径必须立即 `return`，不得执行 `mark_uid_enqueued()` 或清除 provenance。

provenance不得在 redirect结束、普通 owner re-admission中途、control action启动、terminal或release时清除。

### 4.5 Reset、Terminal、Release 与 End-check

| 入口 | 规则 |
|---|---|
| `new()` | 无条件将 provenance fields初始化为零；这是对象构造，不做残留审计 |
| `reset_all_tables()` | destructive reset前若 `valid==1`，`COMMON_DATA` fatal并立即 `return`；只有无残留时才继续 reset，并显式把五个字段清零 |
| `service_active_control_barrier()` 的 `WAIT_OLDER_ROB_COMMIT` 分支 | 在 `bind_control_owner(status)` 和 control state转移前要求 provenance invalid；valid时 fatal并立即 `return`，禁止 prefix外 marker启动 action |
| `try_retire_control_committed_uid()` | control转 terminal前要求 provenance invalid；valid时 `CONTROL_COMMIT` fatal并立即 `return` |
| `release_control_barrier_after_terminal()` | release前要求 provenance invalid；valid时 `CONTROL_BARRIER` fatal并立即 `return`，不得释放 barrier或清 provenance |
| `runtime_drain_complete()` | 返回条件增加 `!control_barrier_prefix_external_provenance_valid`，避免残留被当成 drain完成 |
| `request_global_stop_if_done()` | 当 transaction已完成但 provenance仍valid时 fatal并立即 `return`，提供 UID和 origin metadata；不能只靠 timeout暴露 |
| `end_test_check()` | 在遍历 status和最终 `runtime_drain_complete()` 检查前直接审计 provenance；valid时 `COMMON_DATA` fatal并立即 `return`，打印 UID和 origin metadata |

expected-fatal directed test捕获上述 fatal后，必须先证明字段和其他公共状态未被修改，再由测试代码恢复
人工注入的错误字段，最后调用 `reset_all_tables()`；禁止依赖 reset吞掉残留。

### 4.6 读取者清单

| 读取者 | 读取目的 | 是否可修改 |
|---|---|---|
| `apply_redirect_flush_range()` | 判断首次建立或连续 redirect保持；origin仅首次写入 | 仅首次建立可写 |
| `assert_active_owners_within_admission_prefix()` | 唯一 prefix-external owner豁免与每次 redirect重新匹配 | 只读 |
| `restore_control_admission_prefix()` | 验证同一 marker并在成功 restore后清除 | 成功路径清除 |
| `service_active_control_barrier()` 的 `WAIT_OLDER_ROB_COMMIT` 分支 | 在 `bind_control_owner(status)` 前防止 prefix外 marker启动 action | 只读/fatal |
| `try_retire_control_committed_uid()` | 防止 provenance跨入 terminal | 只读/fatal |
| `release_control_barrier_after_terminal()` | 防止释放仍带 provenance的 barrier | 只读/fatal |
| `reset_all_tables()` | reset残留审计与无残留初始化 | 审计后清零 |
| `runtime_drain_complete()` | 防止残留被当成 drain完成 | 只读并返回0 |
| `request_global_stop_if_done()`、`end_test_check()` | 对完成/最终检查路径提供明确残留诊断 | 只读/fatal |

## 5. 函数级 Coding 方案

### 5.1 新增 provenance clear helper

**函数名**：`clear_control_barrier_prefix_external_provenance()`

**抽象功能描述**：统一将 persistent provenance的 valid、UID和 origin metadata清零。只由构造/reset的
无残留初始化路径和成功的 `restore_control_admission_prefix()` 调用。

**函数目的**：避免多个清除点遗漏 origin字段，同时限制正常运行期只能在 restore成功后清除。

**输入**：无。

**输出/副作用**：五个 provenance fields全部归零；不修改 barrier、status、prefix或 redirect状态。

**源码级伪代码**：

```text
function clear_control_barrier_prefix_external_provenance():
  valid = 0
  uid = 0
  origin_redirect = default zero
  origin_old_end_uid = 0
  origin_new_end_uid = 0
```

**中文文字伪代码**：该 helper只做字段归零，不判断调用时机。调用时机由 caller负责；运行期仅
`restore_control_admission_prefix()` 成功后可以调用。terminal、release或非法 reset不能调用它掩盖残留。

### 5.2 Redirect Preserve 与 Provenance Establish Success 契约

#### 5.2.1 修改 preserve helper

**函数签名**：

```systemverilog
function bit preserve_static_control_marker_on_redirect(
    input memblock_uid_t uid,
    input memblock_redirect_payload_t redirect
);
```

**抽象功能描述**：验证本次 redirect scan命中的 UID仍是唯一未启动 static control barrier；函数不修改
status、active map、prefix、cancel record、observation或 provenance。

**返回值契约**：全部验证通过返回 `1'b1`；任一可被 catcher捕获的 fatal分支在报错后立即
`return 1'b0`。为保证 catcher吞掉 fatal后仍能安全返回，不直接复用当前 void `check_uid()`、
`get_status()`、`get_main_transaction()` 或内部调用这些函数的 `uid_is_control_marker()` 所形成的不可传播
错误链；先用 `is_valid_uid(uid)`、`status_by_uid[uid] != null` 和 `main_table_by_uid[uid] != null` 做本函数内
显式 guard，失败 fatal并返回0。静态 marker判断直接读取已确认非空的
`main_table_by_uid[uid].op_class` 并调用纯判定 `is_control_op_class(...)`。

**源码级伪代码**：

```text
function bit preserve_static_control_marker_on_redirect(uid, redirect):
  if !is_valid_uid(uid):
    fatal COMMON_DATA
    return 0
  if status_by_uid[uid] == null:
    fatal COMMON_DATA
    return 0
  if main_table_by_uid[uid] == null:
    fatal COMMON_DATA
    return 0
  status = status_by_uid[uid]
  is_marker = is_control_op_class(main_table_by_uid[uid].op_class)
  if redirect/barrier/is_marker/static-state/owner/action任一条件不满足:
    fatal CONTROL_REDIRECT
    return 0
  return 1
```

#### 5.2.2 新增 provenance 建立 helper

**函数签名**：

```systemverilog
function bit establish_control_barrier_prefix_external_provenance(
    input memblock_uid_t uid,
    input memblock_redirect_payload_t redirect,
    input memblock_uid_t old_end_uid,
    input memblock_uid_t new_end_uid
);
```

**抽象功能描述**：第一次真实 rollback 后，验证 preserve observation对应 redirect前 admission尾部的
唯一 static barrier，且 rollback确实把它推到 prefix外，然后冻结 origin metadata。

**函数目的**：把 `old_end_uid - 1` 限定在首次建立事件，防止后续 consecutive redirect错误重复要求。

**输入**：`uid`、`redirect`、`old_end_uid`、rollback后的 `new_end_uid`。

**输出/副作用**：成功时一次性设置五个 provenance fields并返回 `1'b1`；失败时 fatal后立即
`return 1'b0`，所有字段保持原值。与 preserve helper相同，UID/status/main transaction有效性和 marker
静态类型由本函数内可返回0的显式 guard完成，不能让 catcher吞掉 `check_uid()`、`get_status()`、
`get_main_transaction()` 或 `uid_is_control_marker()` 的 fatal后继续访问无效对象。

**源码级伪代码**：

```text
function bit establish...(uid, redirect, old_end_uid, new_end_uid):
  if !is_valid_uid(uid) or status_by_uid[uid] == null or main_table_by_uid[uid] == null:
    fatal COMMON_DATA
    return 0
  status = status_by_uid[uid]
  is_marker = is_control_op_class(main_table_by_uid[uid].op_class)
  if provenance_valid:
    fatal CONTROL_ADMISSION_PROVENANCE
    return 0
  if old_end_uid == 0 or uid != old_end_uid - 1 or uid < new_end_uid:
    fatal CONTROL_ADMISSION_PROVENANCE
    return 0
  if uid != active_control_barrier_uid or !is_marker:
    fatal CONTROL_ADMISSION_PROVENANCE
    return 0
  if marker不是 active/enq/non-terminal WAIT_OLDER_ROB_COMMIT静态形态:
    fatal CONTROL_ADMISSION_PROVENANCE
    return 0
  if !rob_order_util::rob_need_flush(status.get_rob_key(), redirect):
    fatal CONTROL_ADMISSION_PROVENANCE
    return 0
  一次性写入 valid/uid/origin_redirect/origin_old_end/origin_new_end
  return 1
```

**中文文字伪代码**：helper先验证当前还没有 provenance，随后只在首次 origin rollback场景检查
`uid == old_end_uid - 1`。它再确认 rollback后的 prefix end已经落在 marker之前，并确认 UID就是当前唯一
active static barrier。最后使用完整调用
`rob_order_util::rob_need_flush(status.get_rob_key(), redirect)` 验证该 marker确实被 origin redirect覆盖。
任一 fatal后立即返回，不能留下部分字段；全部通过后一次性冻结 origin metadata。

#### 5.2.3 全部调用点

| 函数 | 调用点 | 返回值处理 |
|---|---|---|
| `preserve_static_control_marker_on_redirect()` | 唯一生产调用点：`apply_redirect_flush_range()` 的 control marker scan分支 | 返回0立即从 `apply_redirect_flush_range()` 返回；返回1后才设置 `preserve_observation_valid/uid` 并 `continue` |
| `preserve_static_control_marker_on_redirect()` | `soft_test_memblock_rob_admission_window_sequence` 的 expected-fatal direct calls | catcher后必须观察返回0，并证明 observation、prefix、cancel record、barrier/status未变化 |
| `establish_control_barrier_prefix_external_provenance()` | 唯一生产调用点：`apply_redirect_flush_range()` 完成真实 rollback并确认首次 marker prefix-external之后 | 返回0立即从 `apply_redirect_flush_range()` 返回；禁止继续 invariant和 cancel/deleted-owner finalize |
| `establish_control_barrier_prefix_external_provenance()` | 同一 software-only sequence 的 expected-fatal direct calls | catcher后必须观察返回0，并证明五个 provenance fields及其他公共状态未变化 |
| `audit_active_rob_window()` | `apply_redirect_flush_range()` 的 before-scan audit以及其余既有生产 caller | 改为 `function bit`；identity、window或frontier检查失败返回0。range收到0立即返回；其他 caller也必须检查，禁止 catcher后继续对应流程 |
| `assert_active_owners_within_admission_prefix()` | `apply_redirect_flush_range()` 的 after-scan invariant以及 directed direct calls | 改为 `function bit`；任一非法 prefix-external owner或 provenance匹配失败返回0。range收到0立即返回，禁止 cancel finalize/deleted-owner/aggregate副作用 |
| `apply_redirect_flush_range()` | 唯一生产调用点：`apply_redirect_flush()` | 返回0立即从 `apply_redirect_flush()` 返回；不得执行 PTW replay清理、redirect queue删除、phase推进、flush/freeze/active redirect清除或 phase idle。返回1后才允许执行原有外层成功清理 |
| `apply_redirect_flush_range()` | software-only sequence可 direct call用于局部失败边界 | 返回0后证明 invariant、cancel finalize和deleted-owner window未继续；该 direct case不能替代真实 wrapper链测试 |
| `apply_redirect_flush()` | 生产调用点：`exception_redirect_replay_handler::advance_active_redirect()` | 返回值必须被显式接收；返回0时 handler立即退出本次 `advance_active_redirect()`，不得在该调用后新增任何 service副作用。正常生产路径不预期0；0只服务 fail-fast/catcher验证 |
| `apply_redirect_flush()` | 既有 software-only `soft_test_memblock_pending_mmio_directed_sequence::prepare_redirect_deleted_owner_window()` | 必须检查返回1后才验证 deleted-owner window；返回0立即结束当前 directed task/触发明确 fatal，禁止将失败链当成成功准备完成 |
| `apply_redirect_flush()` | 本 plan扩展的 ROB admission software-only wrapper failure case | catcher吞指定 fatal后必须观察 wrapper返回0，并证明全部 range外层状态无副作用 |

以上是当前 `rg` 得到的跨层调用链。range内部的完整 catchable helper清单见第5.3.2节。若 coding时发现额外
caller或 fatal-capable callee，必须先回写 plan并重新 review；禁止
`void'(...)` 丢弃任一新增 bit返回值。

### 5.3 修改 `apply_redirect_flush_range()`

**抽象功能描述**：redirect recovery扫描 active admission窗口，清理普通 owner并观察当前 scan是否命中
static barrier；只有真实 rollback首次把 marker推到 prefix外时才建立 persistent provenance。

**函数目的**：区分“看见 marker”和“marker因 rollback成为 prefix-external”两个事件，并支持 marker
恢复前的连续 redirect。

**输入**：现有 `redirect`、active scan begin/end和各 UID status。

**输出/副作用**：成功返回 `1'b1`，现有普通 owner reissue、cancel和 rollback不变；可能首次建立
provenance；已存在 provenance时保持 origin不变。任一可捕获 fatal路径返回 `1'b0`，且不执行其后的
invariant或 cancel/deleted-owner finalize。

**源码级伪代码**：

```text
function bit apply_redirect_flush_range(redirect):
  preserve_observation_valid = 0
  preserve_observation_uid = 0
  执行现有 redirect/cancel anchor前置检查；任一 fatal后 return 0
  if !audit_active_rob_window("apply_redirect_flush_range:before_scan"):
    return 0
  old_end_uid = get_active_scan_end_uid()

  for uid in [begin_uid, old_end_uid):
    status = get_status(uid)
    保持现有 terminal fault head、inactive/terminal路径；每个可失败 helper返回0立即 return 0
    if rob_order_util::rob_need_flush(status.get_rob_key(), redirect):
      if uid_is_control_marker(uid):
        preserve_success = preserve_static_control_marker_on_redirect(uid, redirect)
        if !preserve_success:
          return 0
        preserve_observation_valid = 1
        preserve_observation_uid = uid
        continue
      更新现有 oldest_flushed_uid/found_flushed
      if !prepare_uid_for_redirect_reissue(uid, redirect):
        return 0

  if found_flushed:
    if !rollback_max_enqueued_uid(oldest_flushed_uid):
      return 0
    new_end_uid = get_active_scan_end_uid()
    if !provenance_valid and preserve_observation_valid and
       preserve_observation_uid >= new_end_uid:
      establish_success = establish...(
        preserve_observation_uid, redirect, old_end_uid, new_end_uid)
      if !establish_success:
        return 0

  if !assert_active_owners_within_admission_prefix(
       redirect, "apply_redirect_flush_range:after_scan"):
    return 0

  先执行 cancel/deleted-owner/aggregate的无副作用precheck；任一失败 return 0
  全部precheck成功后一次性提交 cancel record finalize和deleted-owner window状态
  return 1
```

**中文文字伪代码**：scan中调用 preserve helper时先读取 `bit success`。返回0必须立刻退出整个
`apply_redirect_flush_range()`，不得设置 observation、不得扫描后续 UID、不得执行 rollback、assert或
cancel/deleted-owner finalize；只有返回1才记录局部 observation。若没有普通 owner被 flush，函数
不执行 rollback，因此 observation不会转成 provenance，assert也不会要求一个不存在的 provenance匹配。
若有普通 owner被 flush，函数仍调用原有 rollback；只有 provenance尚未建立、当前 scan确实观察到 marker，
且 rollback后 marker真正位于新 prefix外时，才调用建立 helper；establish返回0时同样立即退出，禁止继续
assert和 cancel/deleted-owner finalize。连续 redirect发生时 provenance已经有效，
函数不得覆盖 origin字段；即使本次 scan范围已经不含 marker，也由后续 assert使用 persistent provenance
验证同一 marker。只有 cancel record `active_scan_done`、软件计数 finalize或 deleted-owner window启动、
active cancel record清除和 aggregate检查全部成功后才返回1。before-scan audit或 after-scan invariant返回0时，
range必须在任何 cancel record finalize、deleted-owner window启动和 aggregate commit之前退出。

### 5.3.2 `apply_redirect_flush_range()` 内全部可捕获检查清单

第四轮 review要求不能只修 preserve/establish。以下是 range直接调用或其单步分支内的完整
fatal-capable/catchable清单。coding前再次用 `rg` 对函数体和一层直接 callee核对；新增项必须遵守同一规则。

| 顺序 | 当前检查/调用 | 设计后的接口 | range失败处理与副作用边界 |
|---:|---|---|---|
| 1 | `redirect.valid`、active cancel record、record index/payload/anchor/sample boundary | range内局部bit precheck | 任一失败 fatal后return0；不得调用 audit、advance、scan或任何 cancel finalize |
| 2 | `audit_active_rob_window()`，内部含 active-map identity、`assert_uid_within_rob_admission_window()`、committed frontier检查 | `function bit audit_active_rob_window()`；内部所有失败返回0 | range收到0立即return0；active map、prefix、cancel record、deleted-owner、PTW/redirect wrapper状态保持调用前值 |
| 3 | `advance_terminal_done_uid()`，内部 `get_status()` 可因UID/status非法fatal | 改为 `function bit`或只读计算next frontier后无失败commit；全部caller检查返回值 | range收到0立即return0；terminal frontier不部分推进，不进入scan |
| 4 | scan中 `get_status()`、`uid_is_control_marker()`/`get_main_transaction()` 等UID/status/transaction lookup | 使用显式 `precheck_uid_status_main()` bit，成功后才读取对象；不得直接调用可fatal lookup而忽略状态 | lookup失败return0；不得修改该UID或后续range状态 |
| 5 | terminal fault head的 `preserve_fault_uid_during_redirect()`，其一层调用含 quiesce/capture/cancel/resource/mapping检查 | 改为 `function bit`；一层 fatal-capable callee改bit或使用无副作用precheck + non-failing commit | 失败立即return0；不得扫描后续UID、rollback、invariant、cancel finalize或deleted-owner window |
| 6 | inactive/terminal分支 `remove_uid_from_issue_queues()`，内部 queue target检查 | 改为 `function bit` 或先对三类queue做无副作用一致性precheck再commit删除 | 失败立即return0；不得继续scan和range尾部处理 |
| 7 | `preserve_static_control_marker_on_redirect()` | `function bit` | 失败立即return0；不得记录 observation或继续scan |
| 8 | started-control scan分支本地 fatal | range局部失败返回0 | 不进入普通owner reissue，不继续scan |
| 9 | `prepare_uid_for_redirect_reissue()`，其一层调用含 owner/cancel/TLB/retire/mapping/reset检查 | 改为 `function bit`，并把一层多步 mutation拆成全量precheck后commit；commit段不得再有可捕获fatal | 失败立即return0；不得处理后续UID、rollback、invariant或range尾部状态 |
| 10 | `rollback_max_enqueued_uid()` 的 UID合法性检查 | 改为 `function bit`，先验证再写prefix | 失败return0；prefix不得部分修改，provenance不得建立 |
| 11 | `establish_control_barrier_prefix_external_provenance()` | `function bit` | 失败return0；五个provenance字段保持原值，不调用invariant |
| 12 | `assert_active_owners_within_admission_prefix()` | `function bit` | 失败return0；不得设置 `active_scan_done`、service cycle、finalized、deleted-owner window或清active cancel record |
| 13 | software cancel count容量检查 | range局部bit precheck | 失败return0；cancel record lifecycle字段不变 |
| 14 | `start_redirect_deleted_owner_window()` 的 idle/record lifecycle检查 | 拆成 `function bit precheck_start_redirect_deleted_owner_window()` + 无失败commit | precheck失败return0；window fields和cancel record均不变 |
| 15 | `check_cancel_pending_aggregate()` | `function bit`只读校验 | 必须在清active cancel record和提交finalize之前完成；失败return0，不得以已提交状态离开 |

步骤13--15采用单一尾部事务：先计算将要写入的 `active_scan_done/service_cycle/finalized/window/active_cancel`
目标值并执行全部只读precheck；全部成功后才按既有成功顺序commit。commit段不得包含 `uvm_fatal`。因此
catcher捕获步骤11、13或14的fatal后，不会出现半完成cancel record或已启动deleted-owner window。

接口落实规则如下：

- `audit_active_rob_window()`、`assert_active_owners_within_admission_prefix()`、
  `preserve_static_control_marker_on_redirect()`、provenance establish、
  `prepare_uid_for_redirect_reissue()` 和 `rollback_max_enqueued_uid()` 在本路径直接改为bit；其生产caller只有
  range，所有directed caller也必须检查返回值。
- 对 `advance_terminal_done_uid()`、`preserve_fault_uid_during_redirect()`、`remove_uid_from_issue_queues()`、
  `start_redirect_deleted_owner_window()` 和 `check_cancel_pending_aggregate()` 等存在无关共享caller的helper，
  新增range专用checked-bit入口；range不得再调用legacy void入口。checked入口是该range事务的唯一事实源，
  不允许“先调用legacy mutation、再做audit”的包装方式。
- `apply_redirect_flush_range()` -> `apply_redirect_flush()` ->
  `exception_redirect_replay_handler::advance_active_redirect()`、pending-MMIO directed和ROB admission directed是
  完整失败传播链。每层收到0立即return；任何层不得 `void'(...)` 丢弃。

本问题的directed强制覆盖两个第四轮指定注入点：

1. before-scan `audit_active_rob_window()` identity/window fatal：range、wrapper及生产caller均返回0；无任何
   scan、cancel/deleted-owner、PTW、redirect queue、phase、flush/freeze或active redirect副作用。
2. after-scan `assert_active_owners_within_admission_prefix()` fatal：允许fatal之前已按场景合法完成的scan/reissue/
   rollback事实，但从invariant失败点起不得发生 cancel record `active_scan_done/service_cycle/finalized`、
   deleted-owner window、active cancel清除、aggregate commit以及任何wrapper PTW/queue/phase清理。测试以调用
   invariant前快照为基准验证这些字段逐项不变。

### 5.3.1 修改外层 `apply_redirect_flush()` 与全部 caller

`apply_redirect_flush()` 同样改为 `function bit`。源码级伪代码：

```text
function bit apply_redirect_flush(redirect):
  if !redirect.valid:
    fatal COMMON_DATA
    return 0
  range_success = apply_redirect_flush_range(redirect)
  if !range_success:
    return 0

  clear_ptw_wait_replay_by_redirect(redirect)
  clear_redirect_drive_queue()
  redirect_phase = FLUSH_APPLIED
  flush_in_progress = 0
  dispatch_flush_in_progress = 0
  issue_freeze_ack = 0
  active_redirect = 0
  redirect_phase = IDLE
  return 1
```

失败返回点必须位于上述全部外层副作用之前。`exception_redirect_replay_handler::advance_active_redirect()`、
`soft_test_memblock_pending_mmio_directed_sequence::prepare_redirect_deleted_owner_window()` 和新增 wrapper
failure directed case必须检查返回值，不能忽略。生产 handler收到0后立即从本次
`advance_active_redirect()` 返回；directed task收到0后不得继续检查或创建 deleted-owner成功状态。

**关键子函数**：

- `preserve_static_control_marker_on_redirect()`：验证本次 scan中的 marker仍为未启动静态 barrier。
- `rollback_max_enqueued_uid()`：保持现有回退算术，增加bit失败传播，按最老flushed普通owner回退prefix。
- `establish_control_barrier_prefix_external_provenance()`：仅首次 origin rollback建立 provenance。
- `assert_active_owners_within_admission_prefix()`：唯一 prefix-external语义豁免点。

### 5.4 修改 `assert_active_owners_within_admission_prefix()`

**抽象功能描述**：每次 redirect scan结束后验证 active ROB map中没有非法 prefix-external owner。唯一
允许项是与 persistent provenance和当前 active static barrier完整匹配的一个 marker。

**函数目的**：在连续 redirect中重复验证同一合法 marker，同时保持普通 owner和 started control的
fail-fast invariant。

**输入**：`redirect`、`caller`。不再输入 `old_end_uid` 或 scan-local proof。

**输出/副作用**：无公共状态修改；合法时返回 `1'b1`；非法时 `ROB_ADMISSION_PREFIX` fatal并立即返回
`1'b0`。

**源码级伪代码**：

```text
function bit assert_active_owners_within_admission_prefix(redirect, caller):
  new_end_uid = get_active_scan_end_uid()
  provenance_match_count = 0

  for each active ROB map entry:
    uid = mapped uid
    if uid < new_end_uid:
      continue
    status = get_status(uid)

    allow = provenance_valid
            and uid == provenance_uid
            and active_control_barrier_valid
            and uid == active_control_barrier_uid
            and uid_is_control_marker(uid)
            and status.active and status.enq and !status.terminal_done
            and status.control_state == WAIT_OLDER_ROB_COMMIT
            and !status.control_action_enqueued
            and !status.control_owner.valid
            and !status.active_lq_mapped
            and !status.active_sq_mapped
            and rob_order_util::rob_need_flush(status.get_rob_key(), redirect)

    if allow:
      provenance_match_count++
      continue

    fatal ROB_ADMISSION_PREFIX with provenance/barrier/status diagnostics
    return 0

  if provenance_valid and provenance_match_count != 1:
    fatal ROB_ADMISSION_PREFIX with stale/missing provenance diagnostics
    return 0
  return 1
```

**中文文字伪代码**：函数不再检查 `old_end_uid - 1`，因为该条件只属于首次建立 provenance。每次
redirect都重新遍历 prefix外 owner，并要求唯一 owner同时匹配 provenance UID、active barrier UID、
static marker状态、无 owner/action、无 LQ/SQ mapping，并被当前 redirect通过完整
`rob_order_util::rob_need_flush(status.get_rob_key(), redirect)` 判断覆盖。普通 owner、错误 provenance、
started marker或第二个 prefix-external owner会立即 fatal并返回0。provenance有效但没有恰好一个匹配
owner也 fatal并返回0，防止 stale provenance。provenance无效且没有 prefix外 owner时返回1，覆盖 B2。

### 5.5 修改 `restore_control_admission_prefix()`

**抽象功能描述**：普通 owner re-admission到 static marker后，恢复 admission prefix并结束该 marker的
prefix-external生命周期。

**函数目的**：把 provenance清除与真正跨回 marker的成功动作绑定，禁止 redirect结束时提前清除。

**输入**：现有 `uid`及 provenance fields。

**输出/副作用**：成功时先调用现有 `mark_uid_enqueued(uid)`，再清除 provenance；失败时 fatal并 return，
不改 prefix和provenance。

**源码级伪代码**：

```text
function restore_control_admission_prefix(uid):
  status = get_status(uid)
  if 现有 static marker条件不满足:
    fatal CONTROL_ADMISSION
    return
  if !provenance_valid or provenance_uid != uid:
    fatal CONTROL_ADMISSION_PROVENANCE
    return
  mark_uid_enqueued(uid)
  clear_control_barrier_prefix_external_provenance()
  更新 last_event_cycle
```

**中文文字伪代码**：函数先完成现有 barrier和status检查，再要求 provenance明确绑定同一 UID。任何 fatal
都立即 return。只有 `mark_uid_enqueued()` 正常完成后才清除 provenance，表示 marker已经重新进入
admission prefix；之后 control action才能启动。

### 5.6 Terminal、Release、Reset 与 End-check 修改

**抽象功能描述**：这些入口不负责恢复 prefix，只负责防止 provenance越过生命周期边界或被静默清理。

**函数目的**：发现 restore缺失时在最早边界 fail-fast，而不是等 timeout或把残留当成正常完成。

**输入**：persistent provenance、active barrier和当前 caller状态。

**输出/副作用**：非法残留时 fatal并立即 return；合法 reset初始化时清零；drain predicate将 valid视为未完成。

**源码级伪代码**：

```text
new():
  clear provenance fields directly

reset_all_tables():
  if provenance_valid:
    fatal COMMON_DATA
    return
  执行现有 reset
  clear_control_barrier_prefix_external_provenance()

service_active_control_barrier() WAIT_OLDER_ROB_COMMIT branch:
  在 bind_control_owner(status) 前:
    if provenance_valid:
      fatal CONTROL_ADMISSION_PROVENANCE with origin metadata
      return
  执行现有 bind_control_owner(status) 和 control state转移

try_retire_control_committed_uid() / release_control_barrier_after_terminal:
  if provenance_valid:
    fatal matching control ID
    return
  执行现有动作

runtime_drain_complete():
  在现有条件中增加 !provenance_valid

request_global_stop_if_done():
  if transaction_done() and provenance_valid:
    fatal CONTROL_ADMISSION_PROVENANCE with origin metadata
    return
  执行现有 stop判定

end_test_check():
  if provenance_valid:
    fatal COMMON_DATA with provenance uid/origin redirect/origin old-new end
    return
  执行现有逐 UID、active map和 runtime_drain_complete() 最终检查
```

**中文文字伪代码**：构造函数只建立零值。testcase reset不是合法 cleanup通道；发现 provenance残留时
先 fatal并 return，避免清零掩盖 restore缺失。control action启动检查的精确位置是
`service_active_control_barrier()` 的 `WAIT_OLDER_ROB_COMMIT` 分支：只有 provenance invalid时才能执行
`bind_control_owner(status)` 并离开静态等待状态。terminal和release同样要求 provenance已经由 restore
清除。`runtime_drain_complete()` 把 provenance视为未收敛；`request_global_stop_if_done()` 和
`end_test_check()` 分别对完成判定和最终检查提供明确 fatal，确保能定位 UID和 origin rollback，而不是
永久等待或只留下泛化的 drain error。

### 5.7 可执行的 Per-marker Exactly-once Lifecycle Audit

第三轮review要求exactly-once不是文字目标；第四轮review进一步要求检查必须在mutation之前。每个marker均可由
字段、断言和日志机械核验。审计字段
放入 `status_transaction`，每次 fresh status/reset清零；字段只记录事实，不参与状态机决策：

```text
control_audit_creation_count
control_audit_owner_bind_count
control_audit_csr_enqueue_count
control_audit_csr_start_count
control_audit_csr_completion_count
control_audit_sfence_enqueue_count
control_audit_sfence_start_count
control_audit_sfence_completion_count
control_audit_l2_assert_enqueue_count
control_audit_l2_assert_start_count
control_audit_l2_assert_completion_count
control_audit_l2_release_enqueue_count
control_audit_l2_release_start_count
control_audit_l2_release_completion_count
control_audit_terminal_count
control_audit_barrier_release_count
```

`control_action_generation` 保持既有权威 generation字段，不新增第二个 generation源。每个事件统一采用两阶段
ticket协议：

```text
ticket = precheck_control_lifecycle_event(uid, event, expected payload/state)
if !ticket.valid:
  return 0
执行唯一生产 mutation
record_control_lifecycle_event(ticket)
```

`precheck_control_lifecycle_event()` 是 `function bit`/ticket-returning只读检查：验证当前count为0、前序count、
kind、owner/generation、queue头部或目标状态，以及mutation后应记录的generation；失败fatal后返回invalid，所有
生产字段保持不变。`record_control_lifecycle_event()` 只接受同一调用栈刚生成的ticket，在mutation成功后把对应
count从0置1并输出日志；它不再做可捕获fatal检查，也不修改queue、owner、generation、control state、terminal
或barrier。若生产mutation本身可能失败，必须先完成其全部无副作用precheck，使commit段无fatal；不得在mutation
失败后record。由此第二次 creation/enqueue/start/completion/terminal/release在任何生产mutation发生前立即失败。
每个post-record输出一条固定格式日志：

```text
CONTROL_LIFECYCLE_AUDIT event=<event> uid=<uid> kind=<kind>
  generation=<control_action_generation> phase=<csr|sfence|l2_assert|l2_release|terminal|barrier_release>
  create/bind=<n>/<n> enqueue/start/complete=<n>/<n>/<n>
  terminal/release=<n>/<n>
```

#### 5.7.1 两阶段事件落点与mutation边界

| 事件 | mutation前 precheck | 唯一生产 mutation | mutation成功后的 record |
|---|---|---|---|
| creation | fresh marker；所有count=0；目标 ROB/barrier slot可建立 | `activate_control_uid()` 建立 active ROB、静态state和barrier | `creation_count=1` |
| owner bind | creation=1、bind=0、generation当前为0、目标owner slot空、provenance invalid | 一次性执行 generation `0->1` 并写入owner token；若bind需要多字段，先完成全部合法性precheck | `owner_bind_count=1`，记录generation=1 |
| CSR/SFence/L2 ASSERT/L2 RELEASE enqueue | 对应enqueue count=0、前序阶段完成、owner/generation匹配、目标queue容量与payload合法 | queue `push_back()` | 对应enqueue count=1 |
| CSR/SFence/L2 ASSERT start | **peek queue front但不pop**；验证队列非空、front UID/kind/profile/owner/generation、start count=0及前序enqueue=1 | 校验通过后才 `pop_front()` 并写现有inflight/start状态 | 对应start count=1 |
| L2 RELEASE start | **peek release queue front但不pop**；验证owner/generation/phase与count | 校验通过后才pop并写现有release request状态 | release start count=1 |
| CSR completion | start=1、completion=0、observed snapshot及目标next-state合法 | 写入现有runtime snapshot完成字段和 `CONTROL_COMMIT_READY` | CSR completion=1 |
| SFence completion | start=1、completion=0、effective observation及目标next-state合法 | 消费/写入既有effective完成事实并转 `CONTROL_COMMIT_READY` | SFence completion=1 |
| L2 ASSERT completion | assert start=1、completion=0、done-high observation合法，且后续RELEASE enqueue可precheck | 写入ASSERT完成事实；随后RELEASE enqueue走独立两阶段协议 | ASSERT completion=1；再record RELEASE enqueue |
| L2 RELEASE completion | release start=1、completion=0、done-low observation及目标next-state合法 | 写入RELEASE完成事实并转 `CONTROL_COMMIT_READY` | RELEASE completion=1 |
| terminal | 对应action矩阵完整、terminal count=0、owner/generation匹配；预检 `terminal_done/active/map` 的全部目标变更 | `try_retire_control_committed_uid()` 执行terminal status写入与 `retire_active_uid()` commit；commit段不得再fatal | terminal count=1 |
| barrier release | terminal=1、release=0、queue/request/owner/action无残留；预检全部barrier/owner/action clear目标 | 一次性清barrier、owner/action live字段 | barrier release count=1并输出归档日志 |

`try_pop_csr_control_action()` 必须按 `completion_profile/l2_flush_phase` 区分 CSR与 check-store ASSERT，禁止把
两个 action合并为同一计数。所有 `try_pop_*` 必须先peek、precheck，再pop；为空时不计start。enqueue必须
先precheck再push，等待baseline而提前return时不得计数。owner bind禁止先递增generation再检查；terminal禁止
先置 `terminal_done`/retire再审计；release禁止先清barrier/owner再审计。

#### 5.7.2 Duplicate caught-fatal 的原子性验收

每类事件至少构造一次“count已为1，再次调用同一生产入口”的expected-fatal。调用前快照以下字段，catcher
捕获唯一 `CONTROL_LIFECYCLE_AUDIT` fatal且入口返回0后逐项比较：

- CSR、SFence、L2 ASSERT和L2 RELEASE queue的size、front payload和完整元素顺序；start场景证明没有pop。
- `control_owner`全部字段、`control_action_generation`、`control_state`、`control_action_enqueued`及flushSb
  request/completion live字段。
- `active`、`terminal_done`、ROB/LQ/SQ map、`active_control_barrier_valid/uid`和相关status字段。
- 所有 lifecycle counters，包括被重复事件count仍为1、其他count不变。

enqueue duplicate不得新增queue元素；start duplicate不得删除front；owner-bind duplicate不得把generation从1
改为2或替换owner；completion duplicate不得重复消费observation或推进state；terminal duplicate不得改变
active/terminal/map；release duplicate不得清barrier或owner。负向测试在撤销人工注入后再进行正常cleanup。

#### 5.7.3 每种 Marker 的精确期望

| marker kind | create/bind | CSR enq/start/complete | SFence enq/start/complete | L2 ASSERT enq/start/complete | L2 RELEASE enq/start/complete | terminal/release |
|---|---|---|---|---|---|---|
| CSR | 1/1 | 1/1/1 | 0/0/0 | 0/0/0 | 0/0/0 | 1/1 |
| SFENCE | 1/1 | 0/0/0 | 1/1/1 | 0/0/0 | 0/0/0 | 1/1 |
| CHECK_STORE | 1/1 | 0/0/0 | 0/0/0 | 1/1/1 | 1/1/1 | 1/1 |

所有 marker均要求 `control_action_generation==1`，并要求曾绑定的 owner generation也为1。redirect前的静态
preserve/restore不得增加 generation或任一 action计数。

#### 5.7.4 Terminal、Release 与 End-test 断言

新增 `assert_control_lifecycle_audit_complete(uid, expect_released, caller)`：

1. 先按上表检查 marker kind对应计数，所有不相关 action计数必须为0。
2. terminal入口使用 `expect_released=0`，先完成两阶段precheck；只有 precheck通过才执行terminal/retire
   mutation，成功后record `terminal=1`。
3. barrier release入口再次检查 action矩阵和 `terminal=1/release=0`，并扫描三条 action queue、L2 release
   queue和 owner flushSb request/completion，禁止存在匹配当前 owner的残留；要求
   `control_action_enqueued=0`。
4. release先完成两阶段precheck，再一次性清除
   `status.control_owner`、`status.control_action_enqueued`、flushSb queued/live字段和 active barrier；成功后record
   `barrier_release=1`并归档generation。历史 counters
   与 `control_action_generation=1` 保留到 end-test供审计。
5. `end_test_check()` 对主表中每个 control marker调用同一 helper的 final模式，要求完整矩阵、
   `terminal/release=1/1`、`active=0`、`control_owner.valid=0`、`control_action_enqueued=0`、无匹配 queue/request、
   无 active barrier；并输出每个 marker的 final `CONTROL_LIFECYCLE_AUDIT` summary。
6. `control_action_drain_complete()` 继续检查全局 queue为空；per-marker helper负责证明没有“队列空但动作重复或
   漏执行”的假 PASS。两者缺一不可。

任何 assertion失败必须使用固定 report ID `CONTROL_LIFECYCLE_AUDIT` 并包含 uid、kind、generation及全部计数，
从日志可直接证明每个 action恰好一次及零残留 owner/action。

## 6. Directed 与负向 Invariant 设计

### 6.1 Expected-fatal catcher 规则

在 `soft_test_memblock_rob_admission_window_sequence.sv` 中将 catcher按预期 report ID参数化。每个负向
场景必须：

1. 只捕获指定 ID；其他 fatal继续抛出。
2. `caught_count` 必须精确为 1。
3. `preserve_static_control_marker_on_redirect()` 与
   `establish_control_barrier_prefix_external_provenance()` 的被捕获 fatal必须返回 `1'b0`；direct call显式断言返回0。
4. `audit_active_rob_window()` 与 `assert_active_owners_within_admission_prefix()` 的被捕获fatal也必须返回
   `1'b0`。before-scan audit case证明scan完全未开始；after-scan invariant case以invariant调用前快照为基准，
   证明cancel/deleted-owner尾部事务完全未开始。
5. 通过 `apply_redirect_flush_range()` 触发任一第5.3.2节失败时，断言 caller在收到0后立即 return：不得执行
   失败点之后的scan、rollback、invariant、cancel record `active_scan_done`/service cycle/finalized、active cancel
   清除、deleted-owner window或aggregate commit。
6. 通过真实 `apply_redirect_flush()` wrapper触发同一失败时，断言 range返回0传播为 wrapper返回0；PTW wait
   replay、redirect drive queue/inflight、redirect phase、flush/freeze、active redirect、active cancel record、
   cancel finalize/deleted-owner状态逐项等于调用前快照。
7. 其他被测 helper以及 `service_active_control_barrier()` 新增守卫的 fatal分支必须返回0或在其void task中
   立即return；不得继续其后的生产mutation。
8. direct helper case在 catcher删除后检查 prefix、provenance、barrier、status和map未变；apply-level case
   将错误 marker放在第一个 flush-covered candidate，或以 helper返回点快照为基准，检查没有 fatal之后的
   observation、assert、cancel/deleted-owner finalize副作用，不要求回滚 fatal之前已经合法完成的动作。
9. lifecycle duplicate case必须在生产入口的precheck阶段fatal并返回0；queue/owner/generation/status/count采用
   第5.7.2节快照逐项相等，不允许“fatal正确但mutation已发生”。
10. 测试代码先撤销人工错误注入，再调用 `reset_all_tables()`；不能用 reset直接吞错误 provenance。

### 6.2 Positive A：首次 rollback 建立并 restore

```text
admit older loads和一个 WAIT_OLDER_ROB_COMMIT CSR marker
-> older fault redirect覆盖年轻普通 load和 marker
-> scan观察 marker并flush至少一个普通 load
-> rollback使 marker成为 prefix-external
-> 首次建立 persistent provenance，origin_old_end满足 marker_uid == old_end_uid - 1
-> invariant恰好匹配同一 marker
-> 普通 load顺序 re-admission
-> restore_control_admission_prefix(marker_uid)
-> prefix跨回 marker后 provenance全部清零
```

检查 marker `dynamic_epoch`不变、control owner/action仍未启动、origin metadata在 restore前稳定。该 software-only
case在 restore和 provenance clear后结束，不启动真实 CSR worker，不声明 action enqueue、terminal或barrier
release结果。

### 6.3 Positive B：Marker 恢复前连续 Redirect

```text
执行 Positive A到首次 provenance建立，但暂不restore marker
-> 部分普通 load重新 admission，prefix仍位于 marker之前
-> 第二个更老 redirect再次flush已重新 admission的普通 load
-> marker不在第二次 active scan范围，preserve observation可为0
-> 既有 provenance保持valid，五个 origin字段完全不变
-> 第二次 invariant使用当前 redirect重新验证同一 prefix-external marker并PASS
-> 可选第三次 redirect重复上述过程，证明不依赖单次scan局部状态
-> 最终普通 load全部re-admission
-> restore marker并清除 provenance
```

该场景是 B1 的主验收用例。每次 redirect都必须使用
`rob_order_util::rob_need_flush(status.get_rob_key(), redirect)` 验证 marker被当前 redirect覆盖。
该 software-only case在 restore/clear处结束；不启动 control worker，不验证 action、terminal或release。

### 6.4 Positive C：只有 Marker、无普通 Flush、无 Rollback

```text
marker仍位于当前 admission prefix内，provenance invalid
-> redirect scan命中 WAIT_OLDER_ROB_COMMIT marker
-> 没有任何普通 owner需要flush，found_flushed=0
-> 不调用 rollback，不建立 provenance
-> marker仍在 prefix内，invariant没有 prefix-external owner
-> 不要求 provenance match，正常返回
```

该场景必须检查 provenance五个字段保持全零，关闭 B2。
该 software-only case只验证没有 rollback、没有 provenance和 invariant正常返回，不验证任何真实控制动作。

### 6.5 可构造负向矩阵

删除“同一次 scan第二个成功 preserve marker”生产测试。保留以下可由 software-only状态明确构造并由
catcher验证的场景：

| 场景 | 预期 fatal | fatal 后必须保持不变 |
|---|---|---|
| 普通 Load/Store owner位于 prefix外且 provenance invalid | `ROB_ADMISSION_PREFIX` | active map、prefix、owner status |
| provenance valid但 UID与 prefix外 marker不一致 | `ROB_ADMISSION_PREFIX` | provenance origin、barrier、prefix |
| provenance UID不是 `active_control_barrier_uid` | `ROB_ADMISSION_PREFIX` | provenance和barrier均不被改写 |
| marker已离开 `WAIT_OLDER_ROB_COMMIT`或 owner/action已启动 | `ROB_ADMISSION_PREFIX` 或对应 control fatal | control state、owner/action generation |
| marker仍有 LQ/SQ mapping | `ROB_ADMISSION_PREFIX` | mapping不得被 helper清理 |
| 当前 redirect不满足 `rob_order_util::rob_need_flush(status.get_rob_key(), redirect)` | `ROB_ADMISSION_PREFIX` | provenance不得刷新为当前 redirect |
| provenance valid但 active map无匹配 prefix-external marker | `ROB_ADMISSION_PREFIX` | provenance不得静默清除 |
| 一个合法 provenance marker之外另有普通 prefix-external owner | `ROB_ADMISSION_PREFIX` | 两个 owner均保持原状态 |
| preserve helper遇到错误 barrier/started marker/无效 status | `CONTROL_REDIRECT` 或 `COMMON_DATA` | helper返回0；apply caller不记录 observation、不执行后续 assert/cancel finalize |
| establish helper遇到错误 UID/origin end/redirect范围/已有 provenance | `CONTROL_ADMISSION_PROVENANCE` 或 `COMMON_DATA` | helper返回0；五字段不变；apply caller不执行后续 assert/cancel finalize |
| before-scan `audit_active_rob_window()` 遇到 map identity/window/frontier错误 | `ROB_ADMISSION_WINDOW` 或 `ROB_ADMISSION_FRONTIER` | audit/range/wrapper/caller返回0；scan、cancel/deleted-owner、PTW、redirect queue、phase、flush/freeze、active redirect均不变 |
| after-scan `assert_active_owners_within_admission_prefix()` 遇到普通owner或错误provenance | `ROB_ADMISSION_PREFIX` | invariant/range/wrapper/caller返回0；从invariant前快照起cancel finalize、active cancel清除、deleted-owner window、aggregate、PTW/queue/phase均不变 |
| 通过真实 `apply_redirect_flush()` wrapper触发 preserve/establish失败 | 对应 helper fatal | wrapper返回0；PTW replay、redirect drive queue/inflight、phase、flush/freeze、active redirect、cancel finalize/deleted-owner状态与调用前完全一致 |
| provenance残留时调用 control action start/terminal/release | 对应 control fatal | 不启动action、不terminal、不release、不清 provenance |
| 同一 marker重复 bind/enqueue/start/completion/terminal/release | `CONTROL_LIFECYCLE_AUDIT` | mutation前precheck立即fatal并返回0；queue/owner/generation/status/count全部等于调用前快照，重复事件count仍为1 |
| provenance残留时调用 `reset_all_tables()` | `COMMON_DATA` | reset不得继续执行；测试撤销注入后再正常reset |

## 7. 验证计划

### 7.1 静态与编译

1. `git diff --check`。
2. `rg`确认 `rollback_max_enqueued_uid()` 仅有void->bit、precheck和return变化，回退算术不变；RTL、Scala和
   RM无修改。
3. 检查五个 provenance fields在 `new()`、`reset_all_tables()`、建立、保持、restore清除、terminal/release、drain/end-check均有明确处理。
4. 检查 `old_end_uid - 1` 只出现在首次建立 helper及其 directed检查，不出现在后续 redirect assert。
5. 全文及源码统一使用 `rob_order_util::rob_need_flush(status.get_rob_key(), redirect)`。
6. 检查 preserve/establish、`audit_active_rob_window()`、`assert_active_owners_within_admission_prefix()`、
   第5.3.2节其他fatal-capable helper、`apply_redirect_flush_range()`、`apply_redirect_flush()` 声明和定义均为
   `function bit`或明确的bit precheck + non-failing commit；全部生产和directed调用点检查返回值。
7. 检查全部bit函数的每个fatal分支均 `return 1'b0`、成功路径均 `return 1'b1`；range收到任一callee 0立即
   return，wrapper收到range 0立即return。`rg`确认range函数体内没有未检查的void fatal-capable调用。
8. `rg`核对 `apply_redirect_flush()` 当前所有 caller：`advance_active_redirect()`、pending-MMIO directed和新增 wrapper failure case；无 `void'(...)` 或未检查调用。
9. 检查lifecycle audit字段在status reset/fresh activation、唯一事件落点、terminal/release/end-test均有明确
   处理；每个事件均为precheck-before-mutation/post-record-after-success，相同事件无第二个record点。
10. 静态检查所有queue start路径使用peek -> precheck -> pop；所有enqueue、owner bind、completion、terminal、
    release均在mutation前precheck，post-record不含fatal或生产状态mutation。
11. 检查其他新增/修改fatal分支后紧跟失败返回，duplicate caught-fatal路径没有mutation。
12. VCS/KDB编译通过，`UVM_ERROR=0`、`UVM_FATAL=0`。

### 7.2 Directed

1. 复用原 `memblock_rob_admission_window_directed_vseq`，不新增 vseq或 cfg；既有边界场景继续 PASS。
2. Positive A 首次建立/restore PASS。
3. Positive B 连续两次及可选三次 redirect PASS，origin metadata保持不变。
4. Positive C 只有 marker、无普通 flush、无 rollback，provenance保持 invalid并 PASS。
5. A/B/C只验 rollback、连续 redirect、invariant match、restore和 provenance clear；不验真实 control worker生命周期。
6. 第6.5节每个负向场景精确捕获一个预期fatal；bit helper返回0，range无失败点之后的
   observation/invariant/cancel finalize/deleted-owner副作用。
7. 单独覆盖 before-scan audit fatal和 after-scan invariant fatal；分别证明全range无动作，以及invariant后的
   cancel/deleted-owner尾部事务无动作。
8. 真实wrapper failure case证明 `apply_redirect_flush()` 返回0，并逐项证明PTW/redirect queue/phase/flush/freeze/
   active redirect及cancel状态无副作用。
9. 对bind、四类enqueue/start/completion、terminal和release执行duplicate caught-fatal原子性测试；queue/owner/
   generation/status/count均与调用前快照一致。
10. software-only不得伪造 lifecycle audit PASS；per-marker action计数只由真实 worker cfg产生。

### 7.3 原始失败与 cfg 全量

使用原 cfg、`basicTest`、`memblock_csr_random_config_vseq`、`MEMBLOCK_MAIN_TRANS_NUM=1000`：

1. seeds `2124399471,1596235805,903730789` 全部 PASS。
2. seed `1611682665` 从头完整运行并 PASS。
3. 原回归该 cfg完整10 seeds全部 PASS：`completed=10 pass=10 fail=0 aborted=0 not_run=0`。
4. 再运行至少30个新随机 seed，全部 PASS。

每个 PASS 必须满足正常 `$finish`、`UVM_ERROR=0`、`UVM_FATAL=0`、无残留 provenance、每个 control marker
均输出 final `CONTROL_LIFECYCLE_AUDIT` summary并满足第5.7.3节精确矩阵、RM compare完成且无残留 active
owner/action/queue/barrier。

### 7.4 真实 Control Worker 生命周期验证

software-only A/B/C不承担 action生命周期验收。以下既有 cfg必须覆盖真实 worker、action exactly once、
terminal和barrier release；不得为本修复新增 vseq或 cfg：

| 既有 cfg | 既有入口 | 本修复验收责任 |
|---|---|---|
| `csr_sfence_check_store_rob_control.cfg` | `basicTest + memblock_dispatch_real_smoke_vseq`，AUTO worker topology；cfg关闭 CSR/SFence并固定插入 check-store | **只负责 CHECK_STORE**：每个 marker必须满足 ASSERT与RELEASE均1/1/1，CSR/SFence计数全0，generation=1，terminal/release=1/1和零残留 |
| `csr_sfence_check_store_manual_control.cfg` | `basicTest + memblock_dispatch_manual_control_vseq`，MANUAL_CONTROL builder显式构造 load -> CSR -> SFence -> check-store | 分别证明一个 CSR、一个 SFENCE、一个 CHECK_STORE marker满足各自精确矩阵；每个 marker generation=1、terminal/release=1/1，所有不相关 phase计数为0 |
| `tc_memblock_csr_random_config.cfg` | `basicTest + memblock_csr_random_config_vseq`；只生成普通 INT_LOAD并随机插入 CSR marker，SFence关闭 | **只负责 redirect后的 CSR lifecycle**：每个随机 CSR marker均满足 CSR 1/1/1、其它 action计数全0、generation=1、terminal/release=1/1；原失败 seeds必须出现 redirect preserve/restore日志且 provenance最终为零 |

验收脚本/人工审计必须从 `CONTROL_LIFECYCLE_AUDIT event=final` 日志按 `uid` 去重计数：final summary数量必须
等于主表 control marker数量；同一 UID只能有一条 final summary。ROB_CONTROL不得声称覆盖 CSR/SFence，CSR
random不得声称覆盖 SFence/check-store。

### 7.5 邻接回归

至少运行：

- `memblock_rob_admission_window_directed_vseq`。
- `csr_sfence_check_store_rob_control.cfg` 和 `csr_sfence_check_store_manual_control.cfg` 的真实 control worker场景。
- `tc_dispatch_real_cancel_reconcile_smoke.cfg`。
- `tc_l2tlb_fault_2pct_no_trigger.cfg` 的 fault redirect场景。
- `tc_memblock_csr_random_config.cfg` 默认请求数 smoke和1000-request matrix。

重点检查普通 prefix-external owner仍 fatal、started control仍 fatal、redirect cancel count不变、普通 Load
re-admission UID顺序不变、连续 redirect origin metadata不刷新。真实 action恰好一次、terminal/release和
最终 provenance为零由第 7.3/7.4 节 real-worker cfg验收，不归因于 software-only A/B/C。

## 8. Coding 顺序与 Review 门禁

1. 独立 reviewer第五轮复审本文，确认首轮B1--B4、二轮B1--B2、第三轮B1--B2和第四轮B1--B2关闭。
2. 测试框架 coding已按用户明确授权和此前 plan review修订实施；不得修改 RTL或Scala。本 worktree中另有
   独立 RM stale-context问题，必须按独立 RM plan/review/commit处理，不属于本 plan实现范围。
3. 先实现 fields、clear/establish helper和生命周期审计，再修改 redirect/assert/restore。
4. 再扩展现有ROB admission directed positive/negative tests；不新增vseq/cfg，并确认range内全部catchable检查、
   range/wrapper/caller返回0后完整调用链无后续副作用，以及lifecycle duplicate在mutation前失败。
5. 完成静态检查、编译、directed、原始 seeds、10 seeds、30随机 seeds和邻接回归。
6. 生成 implementation review；实现偏离时在本 plan增加 `IMPLEMENTATION_DELTA`。
7. 全部验证通过后更新 buglist并将 plan从 `undo` 移到 `do`。

## 9. 验收标准

- 首次真实 rollback建立 provenance；无普通 flush/无 rollback不建立。
- marker恢复前连续 redirect均允许同一 marker，origin metadata不改写。
- `old_end_uid - 1` 只用于首次 provenance建立，后续 redirect assert不依赖该条件。
- restore成功跨回 marker后 provenance全部清零；terminal、release、reset和 end-check不允许残留。
- 唯一语义豁免位于 `assert_active_owners_within_admission_prefix()`。
- 普通 owner、错误 provenance、started marker、错误 redirect范围和多 prefix-external owner继续 fail-fast。
- expected-fatal被catcher捕获后helper返回0，无失败点之后的状态副作用；before-scan audit与after-scan
  invariant均有directed证据。
- preserve/establish/audit/invariant及第5.3.2节全部fatal-capable callee、range/wrapper均返回bit success或使用
  bit precheck + non-failing commit；失败逐层传播，wrapper在PTW/redirect queue/phase/flush/freeze/active
  redirect清理前立即退出。
- `rollback_max_enqueued_uid()` 回退算术、marker dynamic epoch、control owner/action生产语义和DUT激励不变；
  仅新增可传播审计和原子性precheck。
- software-only A/B/C只验admission/provenance；真实action exactly once由per-marker counters、generation断言和
  final结构化日志证明；所有事件使用precheck-before-mutation/post-record-after-success。
- duplicate caught-fatal后queue/owner/generation/status/count逐项不变；start采用peek-before-pop。
- ROB_CONTROL只验固定check-store；MANUAL验CSR/SFence/check-store；CSR random只验redirect CSR lifecycle。
  每个marker满足第5.7.3节精确矩阵且零残留owner/action/queue/barrier。
- 三个失败 seed、一个 aborted seed、原10 seeds、至少30新随机 seeds和邻接回归全部通过。
- 本 control-barrier issue自身无 RTL、Scala/Chisel、RM或DUT接口修改；共享 worktree中的 RM修改必须由独立
  stale-context Buglist/plan/review和独立commit承担。
- 独立 plan review和 implementation review均 PASS。

## 10. RM 与覆盖率协同支持

本 plan不实现 RM/checker/scoreboard，也不修改 RM exception或数据期望。RM只作为回归中确认 Load
compare继续完成的现有观测方。

本 plan不实现 coveragent/covergroup。若后续需要统计，可观测 provenance建立/保持/清除、origin redirect、
连续 redirect次数和 invariant match结果；不得把 coverage采样逻辑放入 prefix invariant。

## 11. Implementation 验证进展（历史迭代记录；最终 closure 见 11.4）

### 11.1 编译与 directed 迭代

- `r7` compile：PASS，`simv` 存在；日志目录
  `mem_ut/ver/ut/memblock/sim/csr_prefix_fix_compile_20261026_r7/`。
- `r7` directed：FAIL，日志
  `mem_ut/ver/ut/memblock/sim/csr_prefix_fix_compile_20261026_r7/log/tc=basicTest_ts=memblock_rob_admission_window_directed_vseq_cfg=default_seed=2601025_rtl_csr_prefix_fix_directed_r7.log`。
  atomicity case之后既有 lifecycle duplicate test在主表已清空时解引用空对象；生产 redirect逻辑未失败。
  修复为 `run_control_lifecycle_duplicate_checks()` 开始时重新 `build_random_main_table(4)`。
- `r8` compile：PASS；`r8` directed：PASS。命令使用
  `basicTest + memblock_rob_admission_window_directed_vseq + cfg=default + seed=2601025`，日志
  `mem_ut/ver/ut/memblock/sim/csr_prefix_fix_compile_20261026_r8/log/tc=basicTest_ts=memblock_rob_admission_window_directed_vseq_cfg=default_seed=2601025_rtl_csr_prefix_fix_directed_r8.log`。
  证据：`TEST_PASS`、caught `UVM_FATAL=5`、uncaught `UVM_ERROR=0/UVM_FATAL=0`、正常 `$finish`。
- 静态 post-commit caller audit发现 `apply_redirect_flush()` 成功尾部的
  `clear_ptw_wait_replay_by_redirect()` 仍调用 report-capable `get_status()`。已改为经过 range precheck后直接读取
  `status_by_uid` 的 commit-only路径；因此最终验证不复用 `r8` 结果。
- `r9`、`r10` compile：INFRA，直接 `make compile` 绕过 `eda01_entry.sh`，导致
  `$MEMBLOCK_XS_HOME` 未展开，无法打开 `mem_ut/ver/common/tcnt_assertion/sva_lib.f`；未进入有效编译，不计功能失败。
- `r11` compile：PASS，入口为
  `mem_ut/ver/ut/memblock/scripts/eda01_entry.sh compile`，有效生成
  `mem_ut/ver/ut/memblock/sim/csr_prefix_fix_compile_20261026_r11/exec/simv`。
  `r11` directed：PASS，日志
  `mem_ut/ver/ut/memblock/sim/csr_prefix_fix_compile_20261026_r11/log/tc=basicTest_ts=memblock_rob_admission_window_directed_vseq_cfg=default_seed=2601026_rtl_csr_prefix_fix_directed_r11.log`；
  证据为 `TEST_PASS`、caught `UVM_FATAL=5`、uncaught `UVM_ERROR=0/UVM_FATAL=0`、正常 `$finish @ 230.800ns`。

### 11.2 当前 caller-chain 审计结论

- `apply_redirect_flush_range() -> apply_redirect_flush() ->
  exception_redirect_replay_handler::advance_active_redirect()` 全链显式检查 bit结果；pending-MMIO directed和ROB
  admission directed wrapper同样检查返回值；未发现 `void'(...)` 丢弃。
- range mutation前完成 terminal frontier、UID mutation、marker唯一性、provenance、planned prefix、cancel容量/
  aggregate和deleted-owner window全量只读precheck。
- mutation commit段调用的 `commit_*`、`release_ptw_wait_replay()`、`clear_pending_store_fault()`、
  `find_cancel_record_index_by_id()`、`status.get_rob_key()`、status clear helpers均不含 report检查；wrapper尾部PTW/
  redirect queue/phase清理由commit-only入口执行。
- directed的 `redirect_before_scan_atomicity` 与 `redirect_late_prefix_atomicity` 对 frontier、ROB/LQ/SQ maps、
  issue/PTW/redirect queues、cancel records/counts、deleted-owner、owner/generation/status、phase/flush/freeze/
  active redirect及完整provenance执行调用前后快照不变检查；5个caught fatal包含这两项和lifecycle duplicate负向项。

### 11.3 r18 预验证与当前 blocker

- `r18` compile PASS。
- ROB admission directed PASS；RM provenance software-only directed PASS。
- stale-context 已知11 seeds 11/11 PASS；原始 prefix 4 seeds 4/4 PASS；fresh random 30/30 PASS。以上结果
  逐日志检查 `UVM_ERROR=0`、`UVM_FATAL=0`、`TEST_PASS`和正常 `$finish`。
- 真实worker邻接：AUTO ROB control、manual control、cancel reconcile均 PASS。
- `tc_l2tlb_fault_2pct_no_trigger` 的10k邻接 `seed=2601202` 已完成1/1 PASS；summary其余失败分类为0，
  最终UVM summary `ERROR=0/FATAL=0`，有`TEST_PASS/$finish`，无threshold kill。结果目录：
  `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/mem_ut/ver/ut/memblock/sim/csr_prefix_fix_r18_adj_l2fault_20261026_234706_results`。
- 原始functional regression 10 seeds已完成10/10 PASS；逐日志最终UVM summary均`ERROR=0/FATAL=0`，有
  `TEST_PASS/$finish`，无threshold kill。结果目录：
  `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_csr_prefix_fix_20261025/mem_ut/ver/ut/memblock/sim/csr_prefix_fix_r18_original10_retry_20261027_002826_results`。
- 独立 reviewer `Popper` 于 `2026-10-27` 给出 `CHANGES_REQUIRED`：现有 software-only duplicate测试只直接
  调用底层 precheck，未证明真实 enqueue/start/completion/terminal/release入口在 catcher吞 fatal后无副作用。
  当前源码已改为调用 CSR生产链的 `enqueue_csr_control_action()`、`try_pop_csr_control_action()`、
  `complete_csr_runtime_snapshot()`、`try_retire_control_committed_uid()`和
  `release_control_barrier_after_terminal()`，并快照queue front/order、owner/generation、state、active/terminal、
  barrier和全部audit counts；该扩展尚待新编译和directed验证。

共享 worktree中同时存在独立 RM stale TLB context修复；其 root PPN compatibility blocker、same-mode root
directed、方案/implementation review和commit均由
`AI_DOC/plan/rm_plan/undo/memblock_rm_stale_tlb_issue_snapshot_context_fix_plan_20261027.md`承担。本 control
plan不得把该 RM预验证结果作为自身 closure。

在新directed、最终源码重编译/复测、完整回归和独立 implementation review全部按
PASS/FAIL/INFRA/INCOMPLETE/ABORTED/NOT_RUN记录前，不移动 plan、不记录 REVIEW_PASS。

### 11.4 r25 exact-tree r4 closure（2026-10-28）

- 验证源码由 `/tmp/prefix_index_20261028` 导出，三个此前受共享 worktree 后续开发影响的关键 blob 为：
  ROB directed `c4a793478cab35102e9a58d5c85aad286fdc71ae`、control service
  `908d1b85f828ba2a7aca9579a098e35f9f8c357a`、common data
  `4e0b4a0fe2e9e9439b517f9c7b8d8f19d22832e4`。
- r4 使用 `basicTest + memblock_rob_admission_window_directed_vseq + cfg=default + seed=2601277`，mode 为
  `prefix_index_gate_r4_20261028`，并强制重新编译；controller compile 和 simulation 均 `rc=0/PASS`。
- controller summary 为 `planned=1 completed=1 pass=1 fail=0 infra=0 incomplete=0 aborted=0`，且
  `remote_unknown_jobs=0`。
- 仿真日志中 `TEST_PASS` 恰好一次，report catcher 统计 `caught UVM_FATAL=19`、caught error/warning 为零，
  最终 report summary 为 `UVM_ERROR=0/UVM_FATAL=0`，并于 `295.100ns` 正常 `$finish`。
- r4 日志：
  `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.prefix_validation_20261028/mem_ut/ver/ut/memblock/sim/prefix_index_gate_r4_20261028/log/tc=basicTest_ts=memblock_rob_admission_window_directed_vseq_cfg=default_seed=2601277_rtl_eda_reg_20261028_205910_3567142_9817.log`。
- controller summary：
  `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.prefix_validation_20261028/mem_ut/ver/ut/memblock/sim/.eda_regression/basicTest_default_20261028_205910_3567142_6471/summary.log`。
- 第 11.1 至 11.3 节只保留为失败定位和迭代过程记录；其中“未完成”“blocker”和“禁止 REVIEW_PASS”均为
  当时状态，不再覆盖本节 closure。当前 single-issue staged scope 已通过静态隔离 review 和 r4 动态门禁，
  可提交并进入母任务分支复测。
