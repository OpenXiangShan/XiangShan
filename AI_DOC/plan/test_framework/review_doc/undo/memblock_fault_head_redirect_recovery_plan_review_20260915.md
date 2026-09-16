# Fault head redirect recovery 计划独立评审（2026-09-15）

| 项目 | 内容 |
| --- | --- |
| 评审对象 | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/AI_DOC/plan/test_framework/plan/do/memblock_fault_head_redirect_recovery_plan_20260915.md` |
| 版本 | V2，`mem_ut_uvm_v2` |
| reviewer | `/root/rtl_liveness_review` |
| 评审结论 | 历史初稿评审；其中 `level=0` 结论已被 2026-09-16 implementation review 的 `level=1` V2 scalar exception 语义取代。 |

## 术语与抽象功能说明

| 术语 | 当前含义 | 代码落点 | 示例 |
| --- | --- | --- | --- |
| fault head | 已由 lsqcommit 驱动且完成 fault terminal 的 ROB head。 | `mark_fault_rob_commit_uid()`、`sync_modeled_head_after_fault_terminal()` | UID5、ROB `0/5`。 |
| redirect reissue | 既有 redirect 对严格年轻 UID 的 cancel 后重新 admission 机制。 | `apply_redirect_flush_range()`、`prepare_uid_for_redirect_reissue()` | UID6 及之后恢复为新 dynamic instance。 |
| flushAfter | `level=0`，仅覆盖 anchor 之后的 ROB 项。 | `rob_need_flush()` | fault UID 不被覆盖。 |
| terminal | transaction 已完成生命周期；fault 的 terminal 为非 success。 | `consume_fault_retire()` | UID5 先完成 terminal。 |
| cancel record | redirect 的唯一 cancel 事实和 monitor anchor 对账状态。 | `cancel_record_q` | 不允许 direct map delete 替代。 |

抽象功能说明：本评审检查 fault head redirect 的时序、状态所有权、年轻 UID 恢复语义和高频路径影响；不审查或修改 RTL。

## 初稿正确部分

方案的入口应是 `sync_modeled_head_after_fault_terminal()`，而不是初次接收到 fault 的 `handle_fault_event()`：前者保证 fault 已到 modeled ROB head，并已完成 `isStoreException` 和 non-success terminal 生命周期；后者没有年龄保证，可能过早覆盖 older instruction。

建议并确认采用既有 generic redirect reissue：fault UID 已 terminal，`flush_itself=0`、`level=0`、`is_vls_exception=0` 的 payload 仅覆盖严格年轻 UID。`apply_redirect_flush_range()` 已通过 cancel record、active map 回收和 admission rollback 维持 `MEMBLOCK_MAIN_TRANS_NUM=100` 的完整工作集；将年轻 UID 直接 terminalize 会让首次约 5% TLB access fault 过早吞掉大部分随机流量，且违反现有 end-test 合同。

计划不直接清 LQ/SQ、不会伪造 deq、不改变 generic redirect owner，也没有新增每拍全表扫描。新 helper 只在一个 fault token 完成时触发一次；后续 active-window 扫描本来就是已有 redirect 的中频路径。

## 必须修订项

### 1. 限定真实 dispatch topology

`sync_modeled_head_after_fault_terminal()` 也被 software-only fault smoke 调用。`soft_test_memblock_dispatch_fault_smoke_sequence.sv` 与 `soft_test_memblock_pending_mmio_directed_sequence.sv` 在调用 `mark_fault_rob_commit_uid()` 后直接完成 synthetic deq，并断言下一 normal head 可提交；它们没有 real redirect driver 或 monitor anchor。

因此无条件建立 fault redirect 会残留 `active_redirect/cancel_record` 并破坏既有软件定向断言。修订版必须以 `memblock_sync_pkg::dispatch_real_smoke_active` 为 gate：为 1 时才申请 fault redirect；为 0 时保留原有 fault terminal/rebase 行为。该 gate 已由真实 dispatch vseq 维护，不新增 plusarg。

### 2. 处理已有 redirect 的重叠生命周期

`sync_modeled_head_after_fault_terminal()` 会由 build、LQ deq、SQ deq 和 raw ctrl deq 路径重复调用。fault terminal 时可能已经有一个合法 generic redirect 在 drive、anchor 或 cancel 阶段。初稿将这种情形直接 fatal，会误报合法重叠。

修订版必须区分：已有 redirect 覆盖 fault token 时不重复申请；已有 redirect 不覆盖 token 时保留 token 和 cursor、等待旧 redirect apply 完成后再申请；没有 active redirect 时才建立本 fault 的 `flushAfter` request。fault ROB key 必须在 token 建立时保存；只有 `request_redirect_flush()` 和 `push_redirect_drive()` 成功后才清 token、推进 cursor。历史已完成但尚待 reconcile 的 cancel record 不应阻止新 request。

## 必查项结果

| 检查项 | 初稿结果 |
| --- | --- |
| fault 入口时序 | 部分通过：只在 fault terminal 后发起，但未限制 topology。 |
| fault UID 语义 | 通过：保持 non-success terminal，不 reissue。 |
| 年轻 UID 语义 | 通过：复用既有 cancel/reissue，维持 100 笔工作集。 |
| 状态 owner | 部分通过：复用既有 owner，但未处理 active redirect 重叠。 |
| 性能 | 通过：无新增高频全表扫描。 |
| RTL 边界 | 通过：不修改 RTL，仅驱动既有 DUT redirect 输入。 |
| 验证 | 需补：两类 software-only 回归和两种 redirect 重叠场景。 |

最终结论：初稿不可作为 coding 依据。修订版已要求写入 topology gate、defer 规则、token ROB key 保存和补充验证；需要最后一轮独立复审无 blocker 后才能进入 coding。
