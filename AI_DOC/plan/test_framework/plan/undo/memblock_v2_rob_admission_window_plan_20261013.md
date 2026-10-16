# MemBlock V2 ROB Admission Window 上游限流与问题3旧修复撤销方案

日期：2026-10-13

状态：设计方案，待 review；本文件不包含代码修改、仿真结果或 commit。

适用 worktree：

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault3_sq_cancel_20261012
```

## 1. 结论摘要

问题3的上游根因不是 redirect 应当扫描 admission prefix 之外的 owner，而是测试框架允许一个新 UID 在
ROB commit head 尚未推进时跨越完整的 `ROB_SIZE=160` 窗口进入 active/DUT-visible 状态。实际失败中：

```text
fault/redirect head: uid=3955, ROB=1/75
异常存活 owner:    uid=4116, ROB=0/76
UID distance:       4116 - 3955 = 161
```

主表按 UID 每次 `rob_advance(..., 1)`，所以静态 UID/ROB 分配本身连续；错误在运行时 admission 没有执行
真实 ROB 容量限制。只检查完整 ROB key 是否重复，不能阻止距离 160 或 161 的 UID 进入，因为一位 wrap
flag 会生成不同 key。

建议修复：以 `lsq_commit_handler.commit_cursor_uid` 表示的“下一条尚未提交 UID”为权威 ROB commit
frontier，在所有 admission 路径发起 DUT 请求或调用 `activate_uid()` 之前执行：

```text
candidate_uid >= rob_commit_frontier_uid
candidate_uid - rob_commit_frontier_uid < MEMBLOCK_ROB_SIZE
```

距离 159 允许，占满第 160 个 ROB slot；距离 160 必须停发，等待 commit frontier 前进。`activate_uid()`
再执行同一规则的 fail-fast 校验，防止 directed test、control 或 non-LSQ 路径绕过前置限流。

在该不变量成立后，合法 active ROB 年龄范围不会跨越一整个环，`rob_is_after()` 才具有唯一解释；redirect
覆盖集合在 UID 上形成合法连续后缀。redirect 回退 `max_enqueued_uid` 后，理论上不应再存在比新 prefix
末端更年轻、仍 active/DUT-visible 的 LSQ owner。因此必须撤销当前 `active_sq_snapshots + prefix 外
stranded_owner 补扫` 修复，恢复 `apply_redirect_flush_range()` 只扫描合法 admission prefix。

## 2. 当前 worktree 状态

检查时存在以下未提交内容：

```text
M  mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv
M  mem_ut/ver/ut/memblock/seq/seq_pkg.sv
?? mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_stranded_sq_cancel_sequence.sv
?? mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_stranded_sq_cancel_directed_vseq.sv
?? AI_DOC/plan/test_framework/plan/undo/memblock_v2_stranded_sq_cancel_owner_coding_plan_20261012.md
?? AI_DOC/plan/test_framework/review_doc/undo/memblock_v2_stranded_sq_cancel_owner_coding_plan_review_20261012.md
?? AI_DOC/plan/test_framework/review_doc/undo/memblock_v2_stranded_sq_cancel_owner_implementation_review_20261012.md
?? AI_DOC/analysis/source_sv/fault_2pct_status_20261010.md
?? mem_ut/ver/ut/memblock/sim/.eda_remote/basicTest_memblock_dispatch_real_smoke_vseq_base_fun.pid
```

本方案后续 coding 只回退问题3旧实现对应的源码和专项 sequence，不删除 `.eda_remote/*.pid`，不提交
commit，不回退其他分析文档或用户已有改动。旧 plan/review 文档建议保留为历史记录，并在实施后补充
“已被 ROB admission window 方案取代”的状态说明，不应把历史文档删除当作功能回退的一部分。

## 3. 根因分析

### 3.1 静态 UID/ROB 顺序没有错误

`memblock_dispatch_base_sequence::build_random_main_table()` 和 control main-table builder 对每个 UID 执行：

```systemverilog
rob_key = rob_order_util::rob_advance(rob_key, 1);
```

因此从 `uid=3955, ROB=1/75` 连续前进：

```text
uid=4039 -> ROB=1/159，distance=84
uid=4040 -> ROB=0/0，  distance=85
uid=4114 -> ROB=0/74， distance=159
uid=4115 -> ROB=0/75，distance=160
uid=4116 -> ROB=0/76，distance=161
```

关键点不是具体中间展示值，而是 distance=160 已经走完一个完整 ROB 容量，distance=161 已超出合法
窗口。静态映射允许长测试生成任意多 UID 是合理的；运行时不能让这些 UID 在旧 ROB head 未释放时同时
成为 active。

### 3.2 当前 admission 只有 LSQ 容量限制，没有 ROB 容量限制

当前 `lsq_ctrl_model::can_allocate()` 只检查：

```text
lq_free_count >= need_lq
sq_free_count >= need_sq
```

`memblock_lsqenq_dispatch_base_sequence::collect_lsq_candidates()` 也只检查 enqueue width、LQ/SQ free count、
flush/owner-window 和控制屏障。LQ/SQ owner 可以在 writeback/deq/cancel 后释放，随后更年轻 UID 继续 admission；
但 fault ROB head 可能仍停在原地，所以 LSQ free 并不等价于 ROB 有空位。

`common_data_transaction::activate_uid()` 当前只检查：

```text
uid 自身未 active
完整 ROB key 当前没有相同 owner
LQ/SQ key 当前没有相同 owner
```

这只能防止 key 完全相同，不能防止新 UID 已经跨过 160 个 ROB slot。`ROB=1/75` 与 `ROB=0/75` 的完整
key 不同，所以 distance=160 仍可通过现有 collision 检查。

### 3.3 为什么 LSQ 容量较小仍能形成 UID distance=161

“同时存在的 active ROB owner 数量”不等于“当前占用的 LQ/SQ entry 数量”。年轻 load/store 可以先完成
LSQ deq，释放 LQ/SQ slot；其 ROB 提交却受最老 fault head 阻塞。当前模型允许释放的 LSQ slot继续分配给
更年轻 UID，于是 admission tail 不断前进，最终越过完整 ROB 窗口。

因此不能用 `lq_free_count`、`sq_free_count` 或 `uid_by_lq/sq.num()` 推导 ROB occupancy。

### 3.4 为什么问题表现成 prefix 外 stranded owner

第一轮 redirect 使用环形 ROB 比较处理当前 prefix。由于 uid4116 已位于非法的 distance=161，
`rob_is_after(ROB=0/76, redirect=1/75)` 无法从一位 wrap flag 唯一恢复真实 UID 年龄，返回“不覆盖”。
其余被覆盖 UID 触发 `rollback_max_enqueued_uid()`，uid4116 因而落到新 prefix 之外。

第二轮 redirect 的 key 改变后，环形比较又可能把 uid4116 判为覆盖对象；DUT 按物理 SQ 状态取消它，原
prefix scan 看不到它，最终形成 cancel count mismatch。prefix 外 owner 是非法 ROB 窗口形成后的下游症状，
不是应长期支持的新生命周期。

## 4. 设计不变量

### 4.1 权威 ROB frontier

ROB admission 的权威 head 必须是“下一条尚未 ROB commit 的 UID”，即当前
`lsq_commit_handler.commit_cursor_uid`，后文称 `rob_commit_frontier_uid`。

不能使用以下对象替代：

- `terminal_done_uid`：normal store 已 ROB commit 但仍等待 SQ deq 时尚未 terminal；真实 ROB slot 已释放，
  用 terminal frontier 会过度限流甚至造成无谓停顿。
- `uid_by_active_rob` 中最小 UID：该 map 同样可能保留已提交、等待 LSQ deq 的 owner，它是事件归属表，
  不是 ROB occupancy 真源。
- `max_enqueued_uid`：它是 admission tail，不代表 ROB head。
- LQ/SQ free count：它们描述独立物理队列，不描述 ROB commit 顺序。

### 4.2 Admission 容量不变量

对每个准备首次或 redirect 后重新 admission 的 UID：

```text
distance = candidate_uid - rob_commit_frontier_uid
0 <= distance < MEMBLOCK_ROB_SIZE
```

具体边界：

```text
distance=0   : ROB head 本身，可以 admission/reissue
distance=159 : 合法，使用窗口内最后一个 slot
distance=160 : 非法，必须 stall，不能发送 DUT enqueue，也不能 activate
distance>160 : 非法状态，activation 防线必须 fatal
```

`collect_lsq_candidates()` 构造多槽 batch 时逐 UID检查，允许只收集到窗口边界，不得因为 batch 第一个 UID
合法而把后续越界 UID一并发出。

### 4.3 UID 与 ROB key 一致性不变量

当 frontier 和 candidate 都在主表范围内，除检查 UID distance 外，还应检查：

```text
candidate.rob_key == rob_advance(frontier.rob_key, distance)
```

该校验不是用于判断容量，而是及时发现 main-table ROB 分配与 UID 顺序漂移。容量必须使用单调 UID distance，
不能用环形 key反推超过一圈的距离。

### 4.4 Redirect/prefix 不变量

在 ROB window 合法的前提下，UID 顺序、ROB 年龄和 admission 顺序一致。一次 redirect 处理完成并回退
`max_enqueued_uid` 后：

```text
不存在 uid > max_enqueued_uid 的 active && enq owner；
不存在 uid > max_enqueued_uid 的 active_lq_mapped/active_sq_mapped owner；
不存在 prefix 外 DUT_VISIBLE reservation。
```

允许的例外不是“补扫处理”，而应被视为生命周期错误并 fail-fast。这样 redirect 的 owner 集合始终由
`[terminal_done_uid, max_enqueued_uid]` 的合法 admission prefix 表达。

### 4.5 Frontier 单调性

`rob_commit_frontier_uid`：

- runtime reset/main-table reset 后为 0；
- 只随成功 ROB commit/terminal fault 收口向前推进；
- redirect 不回退 commit frontier；
- admission prefix rollback 不修改 commit frontier；
- 不得超过 `main_trans_num`；
- 必须与 `lsq_commit_handler.commit_cursor_uid` 始终一致。

## 5. 建议的数据结构与接口

### 5.1 公共只读镜像状态

建议在 `memblock_dispatch_progress_t` 中增加：

```systemverilog
memblock_uid_t rob_commit_frontier_uid;
```

该字段由 commit handler 单写，admission/common-data 多读。初值固定为 0，不需要 valid bit；main table未
ready 时 admission 本来不会启动。若不希望扩大 `memblock_dispatch_progress_t`，也可以作为
`common_data_transaction` 独立字段，但不能在 admission sequence 和 commit handler 各维护一份无校验副本。

### 5.2 `common_data_transaction` 公共 API

建议新增以下接口：

```systemverilog
function void publish_rob_commit_frontier(
    input memblock_uid_t new_frontier,
    input string caller);

function bit rob_admission_window_has_room(
    input memblock_uid_t candidate_uid,
    input string caller);

function void assert_uid_within_rob_admission_window(
    input memblock_uid_t uid,
    input string caller);

function void audit_active_rob_window(input string caller);
```

职责如下：

- `publish_rob_commit_frontier()`：检查 frontier 单调、不越过主表，然后更新公共镜像；必要时校验 handler
  传入值与当前 commit 事件一致。
- `rob_admission_window_has_room()`：正常 backpressure 接口。candidate 位于合法顺序但 distance 达到160时
  返回 0，不报错；candidate 小于 frontier、UID越界或静态 ROB key 不连续时 fatal。
- `assert_uid_within_rob_admission_window()`：给 `activate_uid()` 使用。任何 distance>=160 都是调用者绕过
  admission gate，直接 fatal。
- `audit_active_rob_window()`：遍历 `uid_by_active_rob` 做低频一致性审计。对 `!status.rob_commit` 的 owner
  要求 UID 位于 `[frontier, frontier+159]`；已 ROB commit 但等待 LSQ deq 的旧 owner允许位于 frontier之前，
  不计入 ROB occupancy。

不建议新增一个独立 ROB free-count。UID 与 main-table ROB key 已经一一连续映射，`frontier/tail distance`
足以表达容量；再维护 free-count会引入 redirect、reissue、control、fault 多条增减路径，增加漂移风险。

### 5.3 Commit cursor 单点更新 helper

建议在 `lsq_commit_handler` 增加私有 helper：

```systemverilog
function void set_commit_cursor_uid(
    input memblock_uid_t new_cursor,
    input string caller);
```

它同时完成：

```text
检查 new_cursor >= old cursor
更新 commit_cursor_uid
调用 data.publish_rob_commit_frontier(new_cursor, caller)
```

所有当前直接赋值必须改走该 helper，包括：

- `reset_lsqcommit_runtime_state()` 的 runtime 初始化；
- `finish_fault_head_terminal()`；
- `advance_commit_cursor_past_done()`；
- `mark_rob_commit_batch()`；
- `mark_control_rob_commit_uid()`。

`mark_fault_rob_commit_uid()` 在 fault redirect/terminal 尚未完成时不能推进 cursor，因此也不能释放 ROB
admission窗口；这正是原 seed 应在 uid4115 之前停住的关键。

## 6. 具体文件与函数修改点

### 6.1 `memblock_dispatch_types.sv`

文件：

```text
mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv
```

修改 `memblock_dispatch_progress_t`：增加 `rob_commit_frontier_uid` 及注释，明确它是下一条尚未 ROB commit 的
UID，不是 terminal frontier，也不是 active map 最小 UID。

### 6.2 `common_data_transaction.sv`

文件：

```text
mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv
```

修改点：

1. `new()`/`reset_all_tables()`：初始化 `rob_commit_frontier_uid=0`。
2. 在 `mark_uid_enqueued()` 附近新增 ROB window 公共 API，便于 admission/prefix 规则集中查看。
3. `activate_uid()`：在写入任何 ROB/LQ/SQ map 之前调用
   `assert_uid_within_rob_admission_window(uid, "activate_uid")`。先校验再修改，避免 fatal 后留下部分 map。
4. `apply_redirect_flush_range()`：撤销 prefix 外 snapshot 补扫，恢复只扫描
   `[get_active_scan_begin_uid(), get_active_scan_end_uid())`。
5. redirect scan 结束后调用 prefix containment 断言；发现 prefix 外 active/enq/LQ/SQ owner时直接 fatal，
   不补记 cancel、不自动 reissue。
6. `request_redirect_flush()` 或 `apply_redirect_flush_range()` 开始处调用 `audit_active_rob_window()`，在
   `rob_need_flush()` 使用环形年龄判断前证明所有未提交 active owner处于合法窗口。
7. `end_test_check()` 增加 frontier 与主表/terminal 状态的最终一致性检查。

### 6.3 `lsq_commit_handler.sv`

文件：

```text
mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv
```

修改点：

1. 新增 `set_commit_cursor_uid()`，禁止分散直接写 cursor。
2. 将上述五处 cursor 初始化/推进改为 helper。
3. `ensure_modeled_rob_deq_ptr_initialized()` 和 `rebase_framework_head_from_commit_cursor()` 校验公共
   `rob_commit_frontier_uid == commit_cursor_uid`。
4. normal batch commit 必须在整个 batch状态成功更新后一次性发布 batch tail+1；不得先发布 frontier再因
   中间 UID失败退出。
5. fault token等待期间保持 frontier 不变；只有 `finish_fault_head_terminal()` 才发布 `fault_head_uid+1`。

### 6.4 `memblock_lsqenq_dispatch_base_sequence.sv`

文件：

```text
mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv
```

修改点：

1. `next_uid_needs_lsq_admission()`：在返回 candidate ready 前调用
   `rob_admission_window_has_room()`；无空间时返回 0，形成正常 stall。
2. `collect_lsq_candidates()`：对 batch 中每个 `uid = next_uid + uids.size()` 单独检查。到达 distance=160
   立即停止收集，保留已合法收集的前半 batch。
3. `admit_control_marker_if_ready()`：control marker也占 ROB slot，调用同一容量检查；不能只限制 LSQ请求。
4. `admit_non_lsq_if_ready()` 复用 `next_uid_needs_lsq_admission()` 检查，不增加例外。
5. `confirm_lsq_candidates()` 在 `commit_allocate()` 前可再次断言 batch 中所有 UID仍在窗口内，防止 driver
   等待期间 commit/redirect 状态变化造成候选漂移。frontier只前进不会把合法 candidate变非法，但该检查
   可以捕获 prefix/epoch异常。

### 6.5 `lsq_ctrl_model.sv`

文件：

```text
mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_ctrl_model.sv
```

`commit_allocate()` 和 `commit_non_lsq_admission()` 不拥有 ROB容量策略，仍由 admission sequence正常 stall；
但它们最终调用 `activate_uid()`，因此自动获得 fail-fast 防线。不要把 ROB free count 混入
`lsq_ctrl_model::can_allocate()`，该函数应继续只表达 LQ/SQ物理资源。

### 6.6 `rob_order_util.sv`

文件：

```text
mem_ut/ver/ut/memblock/seq/base_seq_help/rob_order_util.sv
```

不修改 `rob_is_after()` 和 `rob_need_flush()` 的函数公式。可新增注释，明确调用前置条件：参与比较的未提交
ROB owner必须处于同一个 `<ROB_SIZE` 合法窗口，禁止拿相差一整圈或更多的静态 UID进行年龄推断。

如需要测试辅助，可新增只用于已知小于 `ROB_SIZE` step 的 key连续性 helper；不得尝试从两个环形 key
反推出无限单调年龄。

## 7. 撤销当前问题3旧修复

### 7.1 `common_data_transaction.sv` 回退项

从当前未提交 diff 中删除以下问题3旧实现：

- `MEMBLOCK_SQ_CANCEL_PROVENANCE_DEPTH`；
- `memblock_active_sq_owner_snapshot_t`；
- `sq_cancel_provenance_history_q` 及 new/reset清理；
- `record_sq_cancel_provenance()`；
- `dump_sq_cancel_reconcile_diag()` 以及仅由本旧修复加入的调用；
- `snapshot_dut_visible_sq_owners()`；
- `redirect_sq_owner_snapshot_matches()`；
- `apply_redirect_flush_range()` 中的 `active_sq_snapshots` 局部队列、snapshot调用、prefix 外 foreach、
  `stranded_owner` 日志与补充 `prepare_uid_for_redirect_reissue()`；
- 旧修复为 prefix 外补扫调整的 rollback 条件，恢复为原本 `if (found_flushed)` 后回退最老被 flush UID。

回退后可以保留或重新实现“失败时打印 ROB window/frontier”的通用诊断，但不能保留任何 prefix 外 owner
自动计数、删除或 reissue 行为。

### 7.2 专项 sequence 与注册回退

删除旧实现专属文件：

```text
mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_stranded_sq_cancel_sequence.sv
mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_stranded_sq_cancel_directed_vseq.sv
```

从以下文件移除对应两个 include：

```text
mem_ut/ver/ut/memblock/seq/seq_pkg.sv
```

旧 sequence刻意构造 prefix 外 stranded owner，并验证补扫成功；新不变量下该场景本身非法，不能继续作为
正向测试。其有价值的 wrap/identity边界应迁移成新的 ROB admission window负向或断言测试。

### 7.3 不在回退范围内

- 不删除 `.eda_remote/*.pid`；
- 不修改问题1、问题2 worktree或代码；
- 不扩展 LQ stranded-owner修复；
- 不回退与本问题无关的用户改动；
- 不删除已有问题分析、plan/review历史文档。

## 8. 为什么不能直接修改 `rob_is_after()`

`memblock_rob_key_t` 只有：

```text
1 bit wrap flag + ROB value
```

对于相同的两个 key，仅凭 key本身无法区分：

```text
A. left 是 right 之后、尚未走满一圈的年轻请求；
B. left 是 right 之前的老请求；
C. left 已经比 right 多走了一整圈以上。
```

当前公式：

```systemverilog
different_flag = left.flag ^ right.flag;
compare_value  = left.value > right.value;
return different_flag ^ compare_value;
```

只有在系统保证比较双方处于同一个合法 ROB occupancy 窗口时，结果才有唯一年龄语义。对实际失败：

```text
left=0/76, right=1/75, UID distance=161
```

软件已经违反前置条件。把该输入特判为“after”会破坏另一种合法环形状态下同样 key组合的比较结果，且会
影响 redirect、commit pending、MMIO stale 判定等所有调用者。不存在一个只看这两个有限 key、又能正确
恢复无限 UID代数的公式。

因此正确顺序是：

```text
先用单调 UID/commit frontier保证 occupancy < ROB_SIZE；
再在该合法窗口内使用 rob_is_after() 比较 DUT-facing 环形 key。
```

## 9. 断言与诊断

### 9.1 Admission 前断言

对每个 candidate打印或在 fatal中包含：

```text
candidate_uid
candidate ROB flag/value
rob_commit_frontier_uid
frontier ROB flag/value
distance
MEMBLOCK_ROB_SIZE
max_enqueued_uid/valid
terminal_done_uid
caller
```

正常容量满使用低频 stall诊断，不应每拍刷日志。建议只在首次进入 blocked、frontier变化或超过
`no_progress_warn_cycles` 时打印。

### 9.2 Activation fail-fast

`activate_uid()` 若观察到 distance>=160，使用独立 ID，例如：

```text
ROB_ADMISSION_WINDOW
```

该 fatal必须发生在任何 active ROB/LQ/SQ map写入前。

### 9.3 Frontier 一致性断言

commit handler每次 rebase、commit batch、fault terminal后检查：

```text
data.dispatch_progress.rob_commit_frontier_uid == commit_cursor_uid
```

frontier后退、越过 `main_trans_num` 或跳过未提交 UID均 fatal。

### 9.4 Active owner audit

低频 audit 遍历 `uid_by_active_rob`：

- map UID/status/main ROB key三方一致；
- `!rob_commit` owner不得小于 frontier；
- `!rob_commit` owner与 frontier距离必须 `<160`；
- `rob_commit` owner允许因 LSQ deq尾流位于 frontier之前，但不得反向占用 ROB容量；
- 任意 `uid > max_enqueued_uid` 的 active/enq owner在 redirect完成后 fatal。

### 9.5 Prefix containment 断言

`apply_redirect_flush_range()` 完成原 prefix scan和 rollback后，遍历有限 active maps做验证，不做修复：

```text
active ROB/LQ/SQ owner UID <= max_enqueued_uid
```

若失败，日志必须同时给出 redirect key、old/new prefix、owner UID/ROB/LQ/SQ key、frontier和distance，证明是
上游不变量再次被破坏，而不是静默恢复。

## 10. 定向测试方案

建议以新的名字创建专项 sequence，替代旧 stranded-owner正向用例：

```text
soft_test_memblock_rob_admission_window_sequence.sv
memblock_rob_admission_window_directed_vseq.sv
```

### 10.1 基本容量边界

构造 head UID `H` 未提交，静态 ROB key连续：

- `H+159` admission允许；
- `H+160` candidate收集返回 blocked，driver请求中不得出现该 UID；
- 直接调用 `activate_uid(H+160)` 捕获 `ROB_ADMISSION_WINDOW` fatal；
- frontier推进到 `H+1` 后，`H+160` 立即允许。

测试中可通过提前释放中间 UID的 LQ/SQ mapping但不推进 ROB commit来复现“LSQ有空、ROB已满”，避免测试
仅被 LQ/SQ容量先挡住。

### 10.2 Multi-slot batch跨边界

令 next UID距离 frontier为157，enqueue width允许6：

```text
distance 157,158,159 -> 收集
distance 160       -> 停止
```

确认只发送3个合法 slot，剩余 slot valid=0；下一拍 frontier不前进时继续blocked。

### 10.3 ROB wrap

选择靠近 `ROB_SIZE-1` 的起始 key，覆盖：

- same flag/value区间；
- value从159到0且flag翻转；
- distance=159的合法尾部；
- distance=160即使 full key不同也必须blocked。

同时验证 `candidate.rob_key == rob_advance(frontier.rob_key, distance)`。

### 10.4 Fault head长期等待

构造 fault UID `F`：

1. `mark_fault_rob_commit_uid(F)` 建立 `fault_head_waiting`，但不完成 redirect/terminal；
2. 释放年轻请求LSQ资源，让 LQ/SQ free count恢复；
3. admission最多推进到 `F+159`；
4. `F+160` 保持blocked，证明 LSQ free不能绕过 ROB window；
5. 完成 fault redirect和 `finish_fault_head_terminal()`；
6. frontier推进到 `F+1`，原 blocked UID获得一个 slot。

### 10.5 Redirect rollback/reissue

在合法 `<160` 窗口内触发 redirect：

- 被覆盖集合在 UID上是连续后缀；
- `max_enqueued_uid` 回退到最老 flush UID之前；
- 所有后缀 owner均由原 prefix scan删除/reissue；
- redirect结束后 prefix外 active map为空；
- 不需要 active SQ snapshot，不产生 `stranded_owner` 日志。

### 10.6 已提交但 LSQ owner未释放

构造 normal store已 `rob_commit=1`、SQ mapping仍 active：

- commit frontier应越过该 UID，释放 ROB admission slot；
-该旧 SQ owner不计入 ROB occupancy；
- 新 UID仍受新 frontier的 `<160` 限制；
- active ROB map完整 key collision检查继续有效。

该测试用于证明选择 commit frontier而不是 `terminal_done_uid` 或 active-map最小 UID是必要的。

### 10.7 非 LSQ与 control路径

- non-LSQ UID在 distance=160时blocked；
- control marker在 distance=160时blocked，不能建立 control barrier/active ROB map；
- frontier前进后两者均可正常 admission；
- directed helper若直接调用公共 activation API仍被 fail-fast保护。

### 10.8 原问题 seed

重新运行原 seed `676087581` 时重点检查：

```text
uid3955 / ROB1/75 作为 fault head等待期间
uid4115(distance=160)及uid4116(distance=161)不得被 admission/activate/DUT-visible
第一轮 redirect 后不存在 prefix 外 SQ owner
record 3896 不再依赖 ordinary SQ +1 的补扫
LSQ cancel reconcile精确相等
```

若原 seed因新的真实 ROB backpressure改变后续随机时序，不能要求 record编号或绝对 sample保持不变；验收应
依据不变量、最终 UVM结果和 cancel精确对账，而不是旧 record数字。

## 11. 回归验收标准

### 11.1 编译

- VCS compile通过；
- 删除旧 sequence include后无 factory/type残留；
- 新公共 frontier API无 package/include顺序问题；
- `rob_is_after()`/`rob_need_flush()` 无功能 diff。

### 11.2 专项测试

- ROB boundary、batch crossing、wrap、fault wait、redirect reissue、committed SQ tail、non-LSQ/control全部通过；
- distance=160正常路径表现为 stall，不是 fatal；
- 绕过正常 gate直接 activation时稳定触发指定 fatal；
- redirect结束后 prefix containment断言通过。

### 11.3 真实 seed

- seed `676087581` 不再产生 uid4116 prefix外 survivor；
- 原问题3 cancel mismatch消失；
- 不出现新的 ROB key collision、commit cursor drift或永久 no-progress；
- 如果暴露旧实现已记录的独立 LQ生命周期问题，单独归档，不用恢复 SQ补扫掩盖。

### 11.4 基础回归

- 正常无 fault smoke；
- load/store fault smoke；
- control CSR/SFence/check_store topology；
- ROB start zero/mid/near-wrap配置；
- redirect enable/disable支持范围内的现有测试；
- 长 seed验证 frontier持续前进且没有吞吐死锁。

### 11.5 最终状态

- `uid_by_active_rob/lq/sq` 正常 drain；
- `rob_commit_frontier_uid == main_trans_num`；
- `terminal_done_uid == main_trans_num`；
- cancel record和redirect owner window清空；
- 不依赖 prefix外 owner补扫才能通过 end-test。

## 12. 风险与控制措施

### 12.1 过度限流风险

若错误使用 `terminal_done_uid`，已提交 store等待 SQ deq会继续占用模拟 ROB slot，造成吞吐下降或死锁。
控制措施：只使用 commit cursor/frontier，并加入“committed SQ tail”专项测试。

### 12.2 Frontier双写漂移

commit handler私有 cursor与 common-data镜像若分散赋值会不一致。控制措施：所有 cursor写入收敛到
`set_commit_cursor_uid()`，rebase和end-test强校验。

### 12.3 Batch部分接收

窗口边界可能落在一个6-slot batch中间。若只在 batch首项检查仍会越界。控制措施：candidate collection逐项
检查，确认未收集 slot保持 invalid。

### 12.4 Fault cursor释放过早

fault ROB commit事件不等于异常生命周期已经允许 tail复用 slot。若在 `mark_fault_rob_commit_uid()` 立即推进
frontier，会重现原问题。控制措施：保持 fault cursor直到 redirect/terminal收口，仅
`finish_fault_head_terminal()`推进。

### 12.5 旧补扫残留掩盖错误

若保留任何 prefix外自动处理，新限流遗漏仍可能被下游修复隐藏。控制措施：完整删除 snapshot/matcher/专项
测试，prefix外 owner改为断言失败。

### 12.6 随机时序变化

新增 ROB backpressure会改变原 seed的绝对 cycle、record ID和后续随机消费顺序。控制措施：验收比较功能
不变量和最终结果，不绑定旧时间戳；需要保留关键 UID/ROB/frontier日志用于归因。

### 12.7 `rob_is_after()` 其他非法调用者

本方案先约束 admission，但历史代码可能把已跨窗口的静态 key用于其他比较。控制措施：在 redirect入口做
active ROB window audit，并为关键 helper补充前置条件注释；不要用修改比较公式掩盖非法输入。

## 13. 建议实施顺序

1. Review确认 commit frontier语义，尤其 fault head在何时释放 ROB slot。
2. 回退旧 `active_sq_snapshots`、matcher、provenance和旧专项 sequence注册。
3. 增加公共 ROB frontier状态和 commit cursor单点发布接口。
4. 在 LSQ、non-LSQ、control candidate路径加入正常 stall gate。
5. 在 `activate_uid()` 加 fail-fast后置防线。
6. 增加 redirect前 active-window audit和redirect后 prefix containment断言。
7. 新增 ROB admission window专项测试。
8. 编译、专项测试、原 seed和基础回归。

在步骤1 review通过前不应 coding。核心 review问题只有一个：当前框架中真实 ROB slot释放点是否统一由
`commit_cursor_uid`推进表达。现有 normal/control/fault commit生命周期表明答案为是；若后续发现某条提交
路径不经过该 cursor，应先收敛 cursor所有权，再实现 admission限流。
