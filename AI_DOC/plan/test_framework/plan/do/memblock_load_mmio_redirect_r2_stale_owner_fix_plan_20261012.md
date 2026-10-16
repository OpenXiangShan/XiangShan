# MemBlock Load MMIO Redirect R+2 旧 Owner 修复 Plan

状态：已执行并归档；定向测试和正常real-smoke通过，真实失败seed受独立问题1提前阻断

创建日期：2026-10-12

适用版本：V2 `mem_ut_uvm_v2`

## 1. Plan 定位、专有名词与抽象功能说明

本 plan 修复 `mem_to_ooo.loadMmio` 在 redirect 后延迟到达时，测试框架已经删除 active ROB owner，导致 `MMIO_RESOLVE` 无法找到归属并报 fatal 的问题。修改范围只在测试框架的 MMIO owner resolver 和 software-only 定向测试，不修改 RTL，不修改 monitor 对 `mmio_flush_epoch` 的采样含义，也不全局忽略 `loadMmio`。

### 1.1 专有名词

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `loadMmio` | DUT 通知 ROB“某条 scalar Load 是 MMIO”的旁带脉冲，不是 MMIO 数据写回 | `dispatch_raw_ctrl_t.load_mmio_valid[]`、`load_mmio_rob_value[]` | 旧 Load 在 redirect sample 为 `R` 时被杀，但脉冲可能在 `R+2` 才被 monitor 看到 |
| `owner` | 测试框架中把 ROB/LQ/SQ 身份映射到 uid 动态实例的归属关系 | `uid_by_active_rob`、`redirect_deleted_lq_owner_by_key` | active owner 已删除后，只能用受限历史证据证明旧脉冲 |
| `active map` | 只保存当前仍有效动态实例的快速反查表 | `uid_by_active_rob` | 正常 `loadMmio` 仍通过完整 ROB key 的 flag 0/1 probe 走高频路径 |
| `redirect anchor` | monitor 对外部 redirect 的采样序号和 payload 记录 | `redirect_sample_seq`、`redirect_anchor_history_q` | `R` 表示 anchor sample，允许旧 Load 脉冲窗口为 `R..R+2` |
| `redirect record` | 测试框架为一次 redirect 建立的 cancel/flush 生命周期记录 | `cancel_record_q`、`cancel_record_id`、`redirect_epoch` | R+2 旧 owner 必须与唯一 overlap record 一致 |
| `stale-drop` | 已严格证明 raw 属于被 redirect 杀死的旧动态实例，只丢弃该 raw，不更新任何 uid tag | `MEMBLOCK_MMIO_RESOLVE_STALE_DROP` | R+2 的旧 Load 脉冲通过历史证明后返回该结果 |
| `wrap flag` | ROB 环形索引的翻转位；相同 value 的 flag 0/1 是两个不同完整 ROB key | `memblock_rob_key_t.flag` | raw 只有 value，resolver 必须分别考虑两个完整 key，并拒绝歧义 |
| `slow path` | 仅在 active owner 缺失且 raw 位于唯一 redirect overlap 窗口时执行的低频历史扫描 | 新增 helper，由 `resolve_mmio_uid_by_rob_value()` 调用 | 正常 monitor event 不扫描主表；R+2 异常路径才扫描 |
| `historical proof` | 由 status/main table 与现有 redirect-deleted LQ owner 共同组成的旧实例证据 | `status_by_uid`、`main_table_by_uid`、`redirect_deleted_lq_owner_by_key` | 证明候选是本次 redirect 删除的已发射 scalar Load，而不是更早历史或新 owner |

### 1.2 关键函数抽象职责

`resolve_mmio_uid_by_rob_value()`：在 ctrl monitor raw 被转换为 MMIO tag 前，优先通过 active ROB map 解析当前 owner；仅对 Load redirect overlap 的异常情况调用旧 owner 证明 helper。它负责返回 current 或 stale-drop，不能创建、重放或修改动态实例。

`find_unique_redirect_deleted_load_mmio_owner()`：在 R+2 且没有 active owner 时，扫描现有主表/status 的低频历史，并使用现有 redirect-deleted LQ owner 绑定同一次 redirect record；只有唯一候选满足全部证明条件时返回成功。它不写 status、map、queue 或 tag。

`dump_mmio_resolve_failure_context()`：只在最终 fatal 前打印 owner、redirect 和 sample 上下文。它保留前序诊断改动，不参与成功判定。

`soft_test_memblock_pending_mmio_directed_sequence::run_stale_load_overlap_scenario()` 及新增定向 helper：构造 R/R+1/R+2/R+3、无历史 owner、新 active owner、wrap 歧义和非 Load 场景，验证 resolver 的允许边界和 fatal 边界；它们只服务 software-only test，不进入真实 DUT 高频路径。

## 2. 问题根因与目标行为

### 2.1 根因

V2 RTL 中 `loadMmio` 经过 `LoadQueueUncache` 的寄存后输出。外部 redirect 在 sample `R` 被 monitor 观察，LSQ 在 `R+1` 接收 redirect，旧 Load 的残留 `loadMmio` 可以在 `R+2` 才出现。测试框架在 redirect scan 时已经从 `uid_by_active_rob` 删除该 uid，因此 raw 的 ROB value 找不到 active owner。

monitor 在真正观察到脉冲时记录当前 `dispatch_flush_epoch`。因此 R+2 raw 的 epoch 已经是 redirect 后的新 epoch，不能用 `raw_epoch < current_epoch` 的旧 raw 规则自动丢弃。

### 2.2 修改前行为

当前 resolver 只把 `R` 和 `R+1` 识别为 Load redirect overlap。R+2 raw 不进入 overlap 分支；active owner 已删除且 raw epoch 等于 current epoch，最终触发 `MMIO_RESOLVE` fatal。

### 2.3 目标行为

```text
正常 active Load owner：
  继续通过 active ROB map 唯一匹配，并设置 MMIO tag。

R 或 R+1 旧 Load：
  保持现有 active-owner overlap 判定和 stale-drop 行为。

R+2 且没有 active owner：
  进入低频历史证明 slow path。
  只有唯一候选属于同一 redirect 删除的已发射 scalar Load 时 stale-drop。

R+3 及以后、无历史 owner、多个历史候选、存在新 active owner、
wrap flag 歧义、redirect 不覆盖或 kind 不是 Load：
  保持 fatal。
```

## 3. 执行前方案审查与修正

### 3.1 原始最小方案的不足

原始方案希望只扫描 `status_by_uid`，并检查 `load_dispatched` 与 `active_instance_flush_epoch`。当前源码在 `prepare_uid_for_redirect_reissue()` 中调用 `clear_uid_dispatch_result()`，会清除：

- `load_dispatched`
- `active_instance_flush_epoch_valid/active_instance_flush_epoch`
- `load_instance_flush_epoch_valid/load_instance_flush_epoch`

因此 R+2 到达时，单靠 redirect 后的当前 status 无法直接读取旧实例的这些字段，也无法证明某个 inactive status 是“被当前 overlap redirect 删除”而不是更早 redirect 的历史状态。仅扫描 status 并依据 `flushed/issue_killed` 会存在误吞风险。

### 3.2 正式修正方案

不新增长期或第二套 owner map。复用当前 redirect scan 已经建立的单代短时表：

```text
redirect_deleted_lq_owner_by_key
```

该记录在 active owner 删除前保存：

- `uid`
- `old_dynamic_epoch`
- `redirect_epoch`
- `cancel_record_id`

slow path 仍扫描现有 main/status，以恢复完整 ROB key、scalar Load 类型、历史 issue 和当前 inactive/flushed 状态；再用 status 保存的 LQ key查询现有 redirect-deleted LQ owner，证明该 uid 的旧动态实例确实由同一 redirect record 删除。这样不需要新增 ROB tombstone，也不扩大长期状态生命周期。

## 4. Resolver 实现 Flow

### 4.1 overlap 窗口识别

抽象功能描述：`resolve_mmio_uid_by_rob_value()` 在解析 Load raw 前，从有界 `cancel_record_q` 和未绑定 anchor 中选择唯一重叠 redirect；窗口只影响 Load，不改变 Store 处理。

源码级伪代码：

```text
if expected_kind == LOAD:
  对每个有效 redirect record/anchor：
    offset = raw_sample_seq - redirect_sample_seq
    只接受 offset 为 0、1、2
    多个 record/anchor 同时命中则 fatal
  如果命中但 redirect_epoch 无效则 fatal
```

中文文字伪代码：先检查 raw 是否属于 Load。再扫描有界 redirect record 和 anchor，只把采样序号位于 `R..R+2` 的对象视为 overlap。若同一 raw 能被两次 redirect 解释，归属不唯一，立即 fatal。唯一命中后保存完整 redirect payload、epoch、record id 和 anchor sample，供后续 active path 或 slow path 使用。Store 不进入该窗口扩展。

### 4.2 active owner 快速路径

抽象功能描述：resolver 继续分别 probe flag 0/1 的完整 ROB key，从 active map 中解析当前动态实例；正常高频事件不扫描主表。

源码级伪代码：

```text
for flag in {0, 1}:
  key = {flag, raw.rob_value}
  if active map miss:
    continue
  校验 active provenance、op kind 和 dispatch 状态
  if overlap:
    旧实例且被 redirect 覆盖 -> old_covered++
    redirect epoch 内新实例 -> new_candidate++
    其它情况 -> incompatible/uncovered++
  else:
    唯一兼容 owner -> CURRENT
```

中文文字伪代码：resolver 对两个 wrap flag 分别构造完整 ROB key。命中 active owner 后先验证激活 provenance、scalar Load/Store 分类和对应 dispatch 状态。非 overlap 时保持原有 current owner 返回。overlap 时，redirect 前建立且被覆盖的 Load 可以按现有规则 stale-drop；redirect 后建立的新 owner、kind 不兼容或 redirect 不覆盖都不能被丢弃。

### 4.3 `find_unique_redirect_deleted_load_mmio_owner()`

抽象功能描述：该 helper 只在 Load R+2 overlap、active owner 为零时执行，通过一次低频全表扫描寻找唯一的本次 redirect-deleted Load owner。它只返回证明结果和诊断计数，不修改任何公共状态。

函数目的：解决 redirect scan 已删除 active ROB owner后，R+2 `loadMmio` 无法归属的问题，同时避免仅凭时间窗口误吞新请求或旧历史。

输入：

- raw ROB value。
- overlap redirect payload、redirect epoch、cancel record id和 anchor sample。
- 当前 raw sample。

输出/副作用：

- 成功时返回唯一 uid 和完整 ROB key。
- 失败时返回 0，并提供候选计数/原因给 caller 形成 fatal 信息。
- 无 status、map、queue、epoch 或 tag 写副作用。

源码级伪代码：

```text
candidate_count = 0
for uid in [0, main_trans_num):
  跳过空 main/status
  要求 main/status ROB value 等于 raw value
  要求静态行为是 scalar Load 且 commit_is_load
  要求 status 当前 inactive、flushed、issue_killed、redirect_pending
  要求 status.load_issue_epoch != 0，证明旧实例曾真实 issue
  要求完整 status ROB key 被 overlap redirect 覆盖
  要求 status.dynamic_epoch > 0

  用 status 保存的完整 LQ key查询 redirect_deleted_lq_owner_by_key
  要求 owner.valid
  要求 owner.uid == uid
  要求 owner.old_dynamic_epoch + 1 == status.dynamic_epoch
  要求 owner.redirect_epoch == overlap redirect epoch
  要求 owner.cancel_record_id == overlap cancel record id

  candidate_count++
  保存 uid 和完整 ROB key

return candidate_count == 1
```

中文文字伪代码：helper 首先遍历主表和 status，只考虑 raw ROB value相同的记录。它通过主表静态行为确认候选是 scalar Load，通过 `load_issue_epoch` 确认旧实例曾真实发射，通过 inactive、flushed、issue_killed、redirect_pending 确认当前不是可写 tag 的实例，再用完整 ROB key确认该 redirect 确实覆盖它。随后使用 status 中保留的 LQ key查询现有单代 redirect-deleted owner，要求 uid、旧 dynamic epoch、redirect epoch 和 cancel record id全部一致。每个通过的候选计数；只有恰好一个候选才返回成功，零个或多个都由 caller fatal。

### 4.4 R+2 stale-drop 判定

抽象功能描述：resolver 在 active overlap 判定失败后，仅对精确 R+2、无 active owner的 Load调用历史 helper；成功时返回 stale-drop，其他情况继续 fatal。

源码级伪代码：

```text
if load_overlap_observed:
  if 原有 active old owner 唯一证明成立:
    return STALE_DROP

  if raw_sample_seq == redirect_sample_seq + 2 and
     active_candidate_count == 0:
    if find_unique_redirect_deleted_load_mmio_owner(...):
      stale_reason = 包含 uid、完整 ROB key、redirect record 和 R+2
      return STALE_DROP

  fatal("cannot prove LOAD MMIO stale ownership ...")
```

中文文字伪代码：先保留 R/R+1 以及仍存在 active old owner时的原行为。只有 raw 精确位于 R+2 且两个 wrap flag 都没有 active owner，才执行 slow path。helper 唯一命中后，resolver 返回 stale-drop且不写 MMIO tag；helper 零命中、多命中或存在任何 active owner时都 fatal。R+3 不属于 overlap，最终走普通 owner 解析并在无 owner时 fatal。

## 5. 诊断与性能边界

`dump_mmio_resolve_failure_context()` 的前序诊断改动全部保留。新增 helper 属于异常事件 slow path：

- 正常 active `loadMmio/storeMmio`：只查两个完整 ROB key，不扫描主表。
- R/R+1 active overlap：沿用现有有界 record 和 active map 逻辑。
- 仅 R+2、唯一 overlap、active owner 为零：扫描 `main_trans_num` 一次。
- 最终失败：继续调用诊断 dump 全表扫描。

因此高频路径复杂度不变。若未来 R+2 在高压用例中变成高频事件，再考虑建立专用 ROB 历史索引；本 plan 不新增该结构。

## 6. 定向测试 Flow

修改 `soft_test_memblock_pending_mmio_directed_sequence.sv`，复用现有 expected-fatal catcher 和 owner API，不新增 testcase、参数或 filelist入口。

### 6.1 正向场景

1. R 旧 Load：保持现有 stale-drop。
2. R+1 旧 Load：保持现有 stale-drop。
3. R+2 唯一旧 Load：在 redirect record/anchor 建立后调用真实 redirect reissue helper删除 active owner，确认 status 已 inactive且短时 deleted LQ owner存在；注入 R+2 raw，必须 stale-drop且不写 tag。
4. 普通 active Load raw：保持设置 monitor MMIO tag和 `pendingMMIOld` sideband。

### 6.2 预期 fatal 场景

1. R+3 无 active owner：不得使用当前 redirect 解释，必须 fatal。
2. R+2 无历史 owner：必须 fatal。
3. R+2 存在 redirect 后新 active owner：必须 fatal，不得写新 owner tag。
4. R+2 存在 flag 0/1 两个兼容历史候选：必须因 wrap/多候选歧义 fatal。
5. 非 Load owner或 Store raw：不得进入 Load R+2 特判，必须按原 kind 规则处理或 fatal。

每个 negative 场景只在单次 resolver/adapter调用期间安装 report catcher，并匹配对应 `MMIO_RESOLVE` 消息；其它 fatal不得被吞掉。

## 7. 修改文件

计划修改：

1. `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`
   - 保留前序诊断 dump和 `raw_sample_cycle` 参数。
   - overlap窗口扩展到 R+2。
   - 新增只读历史证明 helper。
   - resolver 在精确 R+2且无 active owner时调用 helper。
2. `mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`
   - 保留前序 `raw.cycle` 传参；除非实现 review发现必要，不改变行为。
3. `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_pending_mmio_directed_sequence.sv`
   - 增加 R+2正向和 fatal边界场景。
4. `AI_DOC/mem_ut_flow_doc/virtual_sequence_unified_dispatch_flow.md`
   - 同步 pending-MMIO directed 覆盖从 R/R+1扩展为严格 R+2历史证明。
5. 本 plan及对应 implementation review。

不修改：

- RTL/Scala。
- ctrl monitor 的 `mmio_flush_epoch` 采样。
- MMIO tag setter/query语义。
- Store MMIO resolver时序窗口。
- 参数、cfg、interface、transaction字段和 filelist。

## 8. 验证计划与验收标准

### 8.1 静态检查

```bash
git diff --check
rg -n "resolve_mmio_uid_by_rob_value|find_unique_redirect_deleted_load_mmio_owner" \
  mem_ut/ver/ut/memblock/seq/base_seq_help \
  mem_ut/ver/ut/memblock/seq/base_seq/soft_test
```

### 8.2 定向软测试

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_run tc=basicTest \
  ts=memblock_pending_mmio_directed_vseq \
  cfg=default \
  mode=fault2_mmio_r2_fix_20261012 \
  seed=666666
```

验收：所有正向场景完成；所有预期 fatal 均被精确 catcher 捕获；最终 `TEST CASE PASSED`、`UVM_ERROR=0`、未捕获 `UVM_FATAL=0`。

### 8.3 真实失败 seed

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_run tc=basicTest \
  ts=memblock_dispatch_real_smoke_vseq \
  cfg=tc_l2tlb_fault_2pct_no_trigger \
  mode=fault2_mmio_r2_fix_20261012 \
  seed=37617171
```

验收：原问题2的 `MMIO_RESOLVE` fatal 消失。若运行继续命中问题1或问题3的独立错误，必须按错误 ID、时间和日志上下文单独记录，不能把整条 seed误报为问题2未修复。

### 8.4 正常 smoke

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_run tc=basicTest \
  ts=memblock_dispatch_real_smoke_vseq \
  cfg=tc_dispatch_real_smoke \
  mode=fault2_mmio_r2_fix_20261012 \
  seed=666666
```

验收：`TEST CASE PASSED`、`UVM_ERROR=0`、`UVM_FATAL=0`，正常 active MMIO tag和其它 dispatch flow无回归。

## 9. Review 与归档要求

coding 前先生成本 plan 的正式 review，确认无 blocker。coding 后生成：

```text
AI_DOC/plan/test_framework/review_doc/undo/
  memblock_load_mmio_redirect_r2_stale_owner_fix_implementation_review_20261012.md
```

implementation review 必须覆盖：

- 前序诊断改动保留情况。
- status-only 方案为何不足、为何复用现有 deleted LQ owner。
- active高频路径不扫描主表。
- 所有正向和 negative边界。
- 与本 plan 对齐项、实现差异和测试结果。

coding、文档同步、静态检查和可执行验证完成且 review无 blocker后，将本 plan 从 `plan/undo` 移到 `plan/do`。本任务不创建 git commit。

## 10. 执行中补充/修正（IMPLEMENTATION_DELTA）

### 10.1 deleted-owner 补充旧 activation epoch

[IMPLEMENTATION_DELTA]

来源：coding 中检查 redirect flush后的 status字段清理顺序时，发现 plan中的“activation epoch早于redirect epoch”不能只从 flush后的当前 status恢复。

原 plan：复用现有单代 `redirect_deleted_lq_owner_by_key`，由 status/main table与 deleted-owner联合证明旧动态实例属于同一redirect record；不新建长期owner map。

实现调整：在现有短生命周期 `memblock_redirect_deleted_owner_t` 中增加 `old_activation_flush_epoch`。`capture_redirect_deleted_owner()`只允许捕获仍active、activation epoch有效且严格早于redirect epoch的实例，并在active map删除前把该epoch写入同一deleted-owner记录。R+2 helper要求该值严格小于overlap redirect epoch。

原因：`prepare_uid_for_redirect_reissue()`会清除当前status的activation epoch。若deleted-owner不保存删除前的旧值，R+2时只能证明dynamic epoch和record一致，不能独立证明该实例是在redirect之前激活的旧实例。

影响范围：修改 `memblock_dispatch_types.sv` 的既有短时结构和 `common_data_transaction.sv` 的捕获/读取逻辑；不新建map，不延长deleted-owner窗口，不改变deq owner使用方或清理边界。

### 10.2 directed 场景固定为双 entry 主表

[IMPLEMENTATION_DELTA]

来源：首次专项仿真发现不同场景在RM初始化后把 `main_trans_num` 从1改为2，会触发 `RM_LS_COMMIT` 的主事务数量不变合同。

原 plan：各场景按所需最小transaction数量构造主表，其中wrap歧义场景需要两个entry。

实现调整：所有pending-MMIO directed场景固定构造两个主表entry；单目标redirect统一选择较年轻的uid1，redirect只覆盖uid1。全局停止场景同步完成两个Load的commit/dequeue。

原因：保持整个software-only sequence生命周期内RM主事务数量恒定，同时不改变每个场景要验证的owner关系。

影响范围：只修改 `soft_test_memblock_pending_mmio_directed_sequence.sv` 的测试数据规模和对应uid选择；不改变resolver行为，也不新增测试入口、cfg或filelist。

## 11. 执行结果

- `git diff --check`：通过。
- pending-MMIO directed：`mode=fault2_mmio_r2_fix_v2_20261012 seed=666666`通过，`TEST_PASS`、`UVM_ERROR=0`、未捕获`UVM_FATAL=0`，5个negative场景fatal均被精确catcher捕获。
- 正常real-smoke：`mode=fault2_mmio_r2_fix_smoke_20261012 cfg=tc_dispatch_real_smoke seed=666666`通过，`TEST_PASS`、`UVM_ERROR=0`、`UVM_FATAL=0`。
- 真实失败seed：`cfg=tc_l2tlb_fault_2pct_no_trigger seed=37617171`从`295.900ns`起被独立问题1的scalar fault dual real-WB等待阻断，运行到`566245.900ns`手动停止；期间没有`MMIO_RESOLVE`。待问题1修复后应再次完整复跑该seed。
- 实现review：通过，无coding blocker。本任务未创建git commit。

## 与初步 plan 差异说明

初步方案描述为“R+2时从现有 status历史中扫描唯一旧 Load”。正式 plan保留该低频扫描，但增加了现有 `redirect_deleted_lq_owner_by_key` 作为同一 redirect record 的必要证明。原因是 redirect reissue会清除 `load_dispatched` 和 activation flush epoch，单靠当前 status不能区分“本次 redirect刚删除的旧实例”和更早残留历史。正式方案仍不新增任何 owner map，修改范围保持最小，同时满足不误吞新 owner和 wrap复用事件的安全要求。
