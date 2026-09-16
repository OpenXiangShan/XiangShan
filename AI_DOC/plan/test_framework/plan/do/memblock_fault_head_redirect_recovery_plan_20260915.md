# Fault head redirect recovery 修复计划（2026-09-15）

| 项目 | 内容 |
| --- | --- |
| 状态 | coding、flow 同步、基础/原始复现 seed 与 software-only 回归已完成，已归档到 `plan/do`；仅 generic redirect 与 terminal fault 重叠的专项定向覆盖未单独运行，风险已记录在 implementation review。 |
| 版本 | V2，`mem_ut_uvm_v2` |
| 关联缺陷 | `AI_DOC/buglist/rm/v2/rm_buglist_2026-W38_20260914.md` |
| 分类复核 | `AI_DOC/plan/test_framework/review_doc/undo/memblock_fault_head_redirect_rtl_classification_review_20260915.md` |
| 主要源码范围 | `mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv` |
| 文档同步范围 | `fault_exception_flow.md`、`rob_commit_lq_sq_deq_flow.md`、implementation review |
| 非目标 | RTL、Scala、DUT 接口、L2TLB、RM/scoreboard、plusarg/cfg、直接 LQ/SQ 清理、伪造 deq、普通 memory-violation redirect、STA tombstone 合并逻辑 |

## 专有名词与抽象功能说明

| 术语 | 当前中文含义 | 代码落点 | 本计划示例 |
| --- | --- | --- | --- |
| fault head | 已落 fault、位于 `commit_cursor_uid` 且完成 lsqcommit 驱动的动态 transaction。 | `mark_fault_rob_commit_uid()` | UID5，ROB `0/5`。 |
| fault token | handler 私有等待状态，保存该 fault 的 UID、dynamic epoch 与稳定 ROB anchor，直到它 terminal 或被 older redirect 杀掉。 | `fault_head_waiting` 等字段 | 等待 UID5 的 LQ/SQ deq。 |
| fault terminal | fault 已经过现有 commit/deq 收口，保持 `success=0`、`terminal_done=1`、`active=0` 的非成功终态。 | `consume_fault_retire()` | UID5 不会成为 normal success。 |
| stable ROB anchor | 建 token 时从 status 保存的完整 ROB key；terminal 后不能从已经移动的 cursor 反推。 | `fault_head_rob_key` | 仍为 `0/5`。 |
| redirect level=1 / flush | scalar exception 要驱动给 DUT 的 redirect 等级；DUT 以它作为 flush 类 redirect，而非 `level=0` 的 `flushAfter`。 | redirect payload / `io_redirect_bits_level` | payload `level=1`。 |
| flush itself | 测试框架的 ROB 覆盖语义，设为 1 时 anchor 本身也属于逻辑 flush 范围。 | `payload.flush_itself`、`rob_need_flush()` | fault UID5 已 terminal，扫描时跳过它。 |
| terminal skip | `apply_redirect_flush_range()` 对 `terminal_done` 项不调用 `prepare_uid_for_redirect_reissue()` 的既有行为。 | active-window scan | UID5 不 reissue。 |
| re-admission | redirect 清理年轻旧动态实例后，既有 admission 路径重新建立同一 UID 的新 dynamic instance。 | `prepare_uid_for_redirect_reissue()` | UID6 起重新 admission。 |
| active redirect | 已取得 cancel record、冻结 issue、等待 drive/anchor/apply 的唯一 redirect owner。 | `data.active_redirect` | 不允许并行建立第二个 redirect。 |
| pending monitor redirect | 已进入 `exception_event_q`、但 recovery handler 尚未转换为 active redirect 的真实 monitor event。 | `exception_event_q` | fault redirect 必须让它先仲裁。 |
| topology gate | 只有真实 dispatch vseq 已启动 redirect driver/monitor 时才允许建立 fault redirect 的条件。 | `memblock_sync_pkg::dispatch_real_smoke_active` | software-only fault smoke 为 0。 |

抽象功能描述：本计划只补齐真实 dispatch topology 中 scalar fault 的 recovery 调度。fault 仍先按原有流程 fault commit、RM compare、deq 和 non-success terminal；terminal 后框架驱动一次 `level=1`、`flush_itself=1` 的 redirect。既有 redirect owner 负责 DUT drive、monitor anchor、cancel record、年轻 UID 的 LQ/SQ 回收及 re-admission；fault UID 因已 terminal 被扫描跳过，永不 reissue。software-only topology 保持原有 terminal 后直接推进 cursor 的行为。

## 目标功能 Flow

```text
fault writeback 落表
  -> handle_fault_event() 只消费 recovery event，不发 redirect
  -> fault 到达 modeled ROB head
  -> build_lsqcommit_xaction() 发送 isStoreException sideband
  -> mark_fault_rob_commit_uid() 建立 token 并保存 stable ROB anchor
  -> 既有 fault commit/deq -> consume_fault_retire()
  -> UID5: success=0, terminal_done=1, active=0
  -> sync_modeled_head_after_fault_terminal()
       -> software-only: 清 token，cursor=UID5+1，维持旧流程
       -> real topology:
            pending monitor redirect: 等待 recovery handler 仲裁
            active redirect 覆盖 fault anchor: 标记已覆盖，等待 owner 完成
            active redirect 不覆盖 fault anchor: 保留 token，等待 owner 完成
            无冲突: request_fault_head_redirect()
  -> request_redirect_flush() 建 cancel record/freeze
  -> push_redirect_drive() -> redirect responder 驱动 DUT
       payload = {valid=1, rob=UID5, level=1, flush_itself=1, is_vls_exception=0}
  -> monitor anchor -> apply_redirect_flush_range()
       terminal UID5 跳过，不 reissue
       UID6 及更年轻 active UID 调 prepare_uid_for_redirect_reissue()
       rollback_max_enqueued_uid(UID6)
  -> 既有 admission/issue 从 UID6 重新建立年轻动态实例
```

## 修改前问题

seed `856436350` 中 UID5 的 STA fault 已完成 fault terminal，但 `sync_modeled_head_after_fault_terminal()` 直接把 cursor 推到 UID6，未驱动 `io_redirect_valid`。DUT 因而没有取消 UID6 之后遗留的 LSQ/issue 状态；UID11 的旧动态实例保留了对 UID9 unaligned store 的保守 data-invalid blocker，UID15 持续产生合法 cross-16B replay。该现象由测试框架缺少 fault redirect recovery 闭环引起，独立分类复核已确认不是 RTL 问题。

`level=0` / `flushAfter` 不能用于本计划。它可使软件只重发 younger UID，却不能满足 V2 scalar exception 对 DUT redirect 的 `level=1` 输入语义。`flush_itself=1` 不会让 fault UID 重发：fault 已在请求 redirect 前 terminal，既有 active-window scan 会跳过 `terminal_done` 项。

## 主实现 Flow

### `fault_head_rob_key` 与 `fault_head_redirect_covered`

抽象功能描述：两个 handler 私有字段分别保存 fault token 的稳定 DUT anchor，以及已发现一个 active redirect 覆盖该 anchor 的事实。它们不拥有 LQ/SQ 资源、不参与 normal commit，也不取代 `modeled_rob_deq_ptr`。

文字伪代码：

```text
mark_fault_rob_commit_uid 确认 UID 与 modeled head 一致后：
  保存 status.get_rob_key 到 fault_head_rob_key；
  fault_head_redirect_covered 清零。

fault token 等待 deq、等待冲突 redirect 或申请自身 redirect 期间：
  保持 fault_head_rob_key 不变；
  仅当当前 active redirect 的 rob_need_flush(anchor) 为真时置 covered=1。

token 被 older redirect 杀掉、software-only terminal 完成、已有覆盖 redirect 完成，或自身 redirect 成功申请后：
  统一清除 UID、epoch、ROB anchor 和 covered 标记；
  latched_is_store_exception 保持既有生命周期，不由 token cleanup 重置。
```

### `lsq_commit_handler::request_fault_head_redirect()`

抽象功能描述：新增 helper 把已 terminal 的 real-dispatch fault token 转换为一次 DUT scalar-exception redirect，并交给既有 redirect owner。它不修改 status、cursor、LQ/SQ map、cancel count 或 terminal 状态；只有无 redirect 冲突时才返回成功。

输入：`fault_head_rob_key`、topology gate、`MEMBLOCK_REDIRECT_SEQ_EN`、已有 redirect 状态。

输出/副作用：成功时创建 cancel record、冻结 issue，并把 payload 放入既有 redirect drive FIFO；失败时不改变 fault token，由调用者下一 service tick 重试。

文字伪代码：

```text
确认 topology gate 为 real dispatch；否则调用者不得进入本 helper。
确认 redirect sequence 已启用；关闭时 fatal，避免真实 fault recovery 静默丢失。
若 active redirect、pending drive、全局 flush 或 pending monitor redirect 存在：
  返回 0，不创建第二个 cancel record。

payload 清零后设置：
  valid=1；
  rob_key=fault_head_rob_key；
  level=1；
  flush_itself=1；
  is_vls_exception=0。

调用 request_redirect_flush：
  由 common data 创建唯一 cancel record、推进 flush epoch 并冻结 issue。
调用 push_redirect_drive：
  把同一 payload 放入 redirect responder FIFO；不直接写 DUT interface。
返回 1；后续由 redirect sequence drive、monitor anchor 和 apply_redirect_flush 完成 recovery。
```

### `lsq_commit_handler::mark_fault_rob_commit_uid()`

抽象功能描述：现有函数在 fault UID 的 lsqcommit transaction 已发送后建立唯一 token。它继续保持 fault commit、`isStoreException` latch 和 terminal 时序；本计划只增加 stable ROB anchor 初始化，不在此时提前发 redirect。

文字伪代码：

```text
保留 active、cursor、fault candidate 与 modeled head 的既有一致性检查。
保留 status.rob_commit、fault_head_waiting、fault_head_uid、fault_head_dynamic_epoch 的更新。
读取当前 status.get_rob_key 保存到 fault_head_rob_key，并把 covered 标记清零。
调用既有 try_retire_committed_uid：fault 仍必须等待真实 LQ/SQ deq 后才能 terminal。
调用 sync_modeled_head_after_fault_terminal：如果同拍已经 terminal，进入后续 recovery；否则只继续等待。
最后更新既有 latched_is_store_exception，不改变 sideband 语义。
```

### `lsq_commit_handler::sync_modeled_head_after_fault_terminal()`

抽象功能描述：现有函数观察 fault token 是否仍属于当前 dynamic instance，并在 fault 完成 terminal 后选择旧的 software-only rebase、等待其它 redirect，或申请一次 fault redirect。它不直接操作 cancel record、LQ/SQ resource 或 replay queue。

文字伪代码：

```text
如果没有 fault token：
  保留既有 terminal cursor rebase 行为并返回。

读取 fault_head_uid 的 status。
如果 dynamic_epoch 已变化、status.flushed、status.issue_killed 或 rob_commit 消失：
  说明 older redirect 已杀掉旧动态实例；清 token；
  保持 cursor 在同一 UID，rebase 后等待该 UID re-admission；返回。

如果 fault 尚未满足 terminal_done、lsq_deq、active=0、success=0、fault=1、LQ/SQ map 已释放：
  返回，继续等待既有真实 deq。

如果 topology gate 为 0：
  按旧行为把 cursor 推到 fault_uid+1；清 token；rebase；不创建 redirect。

如果有 pending monitor redirect：
  保留 token 和 cursor；让 recovery handler 先按 oldest redirect 仲裁；返回。

如果有 active redirect：
  若 rob_need_flush(fault_head_rob_key, active_redirect) 为真，记录 covered=1；
  无论是否覆盖都保留 token 和 cursor，等待 active redirect 完整 apply。

如果 covered=1 且不存在 active/pending/global flush：
  已有 redirect 已负责 younger UID recovery；只把 cursor 推到 fault_uid+1，清 token 并 rebase；不再发第二个 redirect。

若没有 redirect 冲突：
  调用 request_fault_head_redirect。
  仅当返回成功时，把 cursor 推到 fault_uid+1，清 token 并 rebase。
  申请成功后 active redirect 会阻塞 admission/issue，直至既有 owner apply 完成。
```

### 既有 `apply_redirect_flush_range()` 的复用边界

抽象功能描述：`common_data_transaction::apply_redirect_flush_range()` 保持 active-window cancel/recovery 的唯一 owner，本计划不修改它。

文字伪代码：

```text
fault redirect 的 payload 是 level=1、flush_itself=1，因此 ROB 语义包含 UID5 anchor 与其后的 UID。
扫描首先跳过 terminal_done 项：UID5 已是 non-success terminal，不调用 prepare_uid_for_redirect_reissue。
对 UID6 及更年轻的 active 项，现有 rob_need_flush 返回真：
  prepare_uid_for_redirect_reissue 记录 cancel、回收旧 active map/issue state、递增 dynamic_epoch；
  rollback_max_enqueued_uid 以最早 UID6 作为恢复边界。
后续 admission 由既有 cursor/active-map owner 从 UID6 开始重新建立；不伪造 deq，不直接删除 map。
```

## 范围与风险控制

- 不在 `handle_fault_event()` 收到任意 fault 时发 redirect；那里没有 fault head 年龄保证。
- 不把 fault UID 重新 admission 或 reissue；它先完成 RM compare 和 `success=0` terminal，随后仅作为 terminal skip。
- 不用 `level=0` 或 `flushAfter` 规避软件重发问题；DUT payload 必须为 `level=1`。
- 不新建全局 hold；已有 `fault_head_waiting` 与 `active_redirect` 已分别保护 fault terminal 和 flush 时段。
- 不修改 `prepare_uid_for_redirect_reissue()`、`apply_redirect_flush_range()`、terminal prefix、LQ/SQ pointer 或 RTL。
- 不把 cross-16B 的 `feedbackSlow.hit=0` 当成新的 TLB miss 或限制 replay 次数；已有 tombstone 合并改动保持不变。
- 不在 `dispatch_real_smoke_active=0` 的 software-only 场景创建 redirect；这些场景没有 DUT redirect responder/monitor anchor。

## 验证计划

1. 静态检查：`git diff --check`；检索 fault redirect 的 `level=0`、`flush_itself=0` 残留，确认新字段均有 reset、设置、读取和清理路径。
2. 基础编译/运行：`tc=basicTest ts=virtual_base_sequence mode=base_fun`。
3. 原始专项回归：使用临时 RTL `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/test-rtl/build/rtl`，运行：

```text
tc=basicTest
ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq
cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k
seed=856436350
+MEMBLOCK_MAIN_TRANS_NUM=100
+MEMBLOCK_HARD_XZ_CHECK_EN=0
+MEMBLOCK_CHECK_TRIGGER_EN=0
```

验收：

- UID5 在 fault terminal 后恰好产生一次 redirect drive，ROB 为 `0/5`、`level=1`、`flush_itself=1`、`is_vls_exception=0`。
- UID5 保持 `success=0`、`terminal_done=1`，不重新入队、不重新发射。
- `apply_redirect_flush_range()` 的最早 reissue UID 为 UID6；UID6 是下一条 admission。
- UID11 的旧 C_FF 实例被 redirect recovery 清理，UID15 不再因其遗留状态进入无限 replay。
- 无 `STA_LATE_TOMBSTONE` overflow、`UVM_ERROR`、`UVM_FATAL`、timeout 或 no-progress，且 `terminal_done_uid=100`。

4. software-only fault smoke 与 pending-MMIO directed fault 回归：topology gate 为 0 时没有 `active_redirect`、cancel record 或 pending redirect drive，原有 fault terminal 后 normal head 行为不变。
5. 定向重叠检查：已有 generic redirect 覆盖 fault anchor 时不再发第二次 fault redirect；不覆盖时 fault token 等待旧 redirect apply 后再申请一次自身 redirect。
6. 代码、flow 文档和 implementation review 已完整同步且自查无 blocker；本 plan 已从 `undo` 归档到 `do`。

## 文档与 Review 产物

- 缺陷记录：`AI_DOC/buglist/rm/v2/rm_buglist_2026-W38_20260914.md`
- RTL 分类 review：`AI_DOC/plan/test_framework/review_doc/undo/memblock_fault_head_redirect_rtl_classification_review_20260915.md`
- 历史 plan review：`AI_DOC/plan/test_framework/review_doc/undo/memblock_fault_head_redirect_recovery_plan_review_20260915.md`。其中 `level=0` 结论已被本 plan 的 V2 scalar exception 语义修订取代。
- coding implementation review：`AI_DOC/plan/test_framework/review_doc/undo/memblock_fault_head_redirect_recovery_implementation_review_20260916.md`

## 与旧方案的差异

旧文本将 fault redirect 设计为 `level=0`、`flush_itself=0` 的 `flushAfter`。该方式只描述软件 younger reissue，未符合已确认的 V2 scalar exception DUT redirect 语义。本修订固定 DUT payload 为 `level=1`，框架 payload 同时固定 `flush_itself=1`；fault 的不重发由 terminal skip 保证，而不是依赖把 anchor 排除在 redirect 覆盖范围外。
