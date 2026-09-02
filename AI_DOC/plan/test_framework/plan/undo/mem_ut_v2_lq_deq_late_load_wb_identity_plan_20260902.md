# V2 LQ Deq 后 Load Writeback 身份保持方案

状态：`undo`，待执行。

本文修复 V2 真实 Load 流中 `lqDeq` 早于对应 Load writeback 时，测试框架把已释放的物理 LQ resource mapping 误判为 writeback 身份缺失的问题。该方案只调整 `dispatch_monitor_event_adapter` 的 Load writeback 快照回填，不修改 RTL、L2TLB PBMT payload、RM、LSQ 指针模型或 Store 路径。

关联失败证据：

- 日志：`mem_ut/ver/ut/memblock/sim/pbmt_dynamic/log/tc=basicTest_ts=memblock_l2tlb_pbmt_response_fault_vseq_cfg=tc_l2tlb_pbmt_response_fault_seed=666666_rtl_pbmt_pmp_endpoint.log`
- 波形：`mem_ut/ver/ut/memblock/sim/pbmt_dynamic/wave/tc=basicTest_ts=memblock_l2tlb_pbmt_response_fault_vseq_cfg=tc_l2tlb_pbmt_response_fault_seed=666666_rtl_pbmt_pmp_endpoint.fsdb`
- 时序：UID0 在 610.3ns issue，682.8ns 观察到 `lqDeq` 并释放 `uid_by_lq`，827.8ns 收到同一完整 ROB key 的 scalar Load WB，现有代码在 `INT_WB_ATTACH` 处 fatal。

## 1. 专有名词与抽象功能说明

| 术语 | 当前含义 | 代码落点 | 示例 |
| --- | --- | --- | --- |
| active LQ mapping | 物理 LQ slot 当前由哪个 active UID 占用的 resource owner map。deq 后必须释放，允许 slot 被后续 UID 复用。 | `common_data_transaction::uid_by_lq`、`status.active_lq_mapped` | UID0 的 `lq=0/0` deq 后 map 不再归 UID0。 |
| persistent LQ identity | UID 建表时冻结在 status 中的 LQ key，不随资源 map 释放而清零。 | `status.lqIdx_flag/lqIdx_value` | UID0 的历史 LQ key 仍可写入其 WB event 用于诊断。 |
| current issue snapshot | adapter 对一个 raw WB 以 ROB key、target issue epoch、replay sequence 和 flush provenance 确认当前动态实例后，填充出的 event 身份。 | `dispatch_monitor_event_adapter::fill_current_issue_snapshot()` | scalar LDA raw 用完整 ROB key 定位 UID0。 |
| LQ deq | DUT 宣告对应物理 LQ resource 已经出队；它不是已完成 Load WB 的同义信号。 | `lsq_commit_handler::commit_dut_lq_deq()` | 682.8ns deq 先释放物理 slot，WB 可在后续周期到达。 |
| late Load WB | LQ deq 后、UID 仍 active 且当前 load issue 未完成时到达的真实 scalar Load writeback。 | `convert_raw_int_wb()` 的 `SCALAR_LDA` 分支 | 827.8ns 的 UID0 WB。 |

`dispatch_monitor_event_adapter::fill_current_issue_snapshot()` 的抽象职责是把一个已经由 ROB key 选中的、仍属于当前动态实例的 raw writeback 转换为完整 `memblock_wb_event_t`。它验证 active/redirect/issue/flush 生命周期并填充 LQ/SQ 身份，但不拥有 LQ/SQ 资源分配或释放。

`common_data_transaction::release_uid_lq_mapping()` 的抽象职责是释放已 deq 的物理 LQ resource owner，防止旧 UID 长期占用 slot。它不清除 status 中的静态 LQ identity，也不判定后续 raw writeback 是否过期。

## 2. 问题边界与目标 Flow

当前错误的隐含条件是：Load WB 到达时 `active_lq_mapped` 必须为 1。真实时序已经证明这个条件不成立：deq 正确释放物理 slot，但 UID 的 ROB mapping、`load_dispatched`、target issue epoch 和 target flush epoch 仍然有效，因此 WB 仍可以被唯一归属。

目标 Flow：

```text
raw scalar Load WB
  -> 用完整 ROB key 取得 active UID
  -> 保留现有 active/terminal/redirect/issue/flush/epoch 校验
  -> 读取 UID 的 persistent LQ identity
  -> active_lq_mapped=1：继续校验 uid_by_lq owner
  -> active_lq_mapped=0 且 lsq_deq=1：允许使用历史 LQ identity，不查询 uid_by_lq
  -> active_lq_mapped=0 且 lsq_deq=0：保持 fatal，拒绝无原因丢失的 resource mapping
  -> 填充 WB event，交由既有 writeback handler 记账
```

非目标：

- 不保留 deq 后的 `uid_by_lq` tombstone，不延迟物理 LQ slot 释放。
- 不放宽 ROB、issue epoch、replay sequence、active instance flush epoch 或 terminal/redirect 校验。
- 不改变 STA/STD 的 SQ owner 校验。
- 不改写 DUT RTL 或修改 L2TLB/RM 对 PBMT 的职责。

## 3. `fill_current_issue_snapshot()` 修改 Flow

### 3.1 Load 分支的身份与资源检查

抽象功能描述：Load 分支在当前 WB 已通过 ROB 和 target 生命周期检查后，区分“物理 LQ 仍活跃”和“已 deq 但 UID 仍等待 WB”两种合法状态。两种状态都使用 status 中冻结的 LQ key 填充 event；只有前者查询 live owner map。

源码级伪代码：

```text
LOAD 分支：
  读取 status 中的 lq key。
  若 lq key 不合法：fatal。

  若 active_lq_mapped：
    查询 uid_by_lq。
    若 map 缺失或 owner 不是当前 uid：fatal。
  否则：
    若 lsq_deq 不是 1：fatal，表示 resource mapping 在非 deq 条件下丢失。
    不读取 uid_by_lq，因为该 slot 可以已被后续 UID 合法复用。

  把 status LQ key 写入 wb_event，置 has_lq。
```

中文文字伪代码：先由函数外层的 ROB key 找到 UID，并验证该 UID active、未 terminal、未被 redirect 杀死且对应 Load 已真实 dispatch。随后外层继续验证当前 issue epoch 和 flush provenance。只有这些身份检查完成后才进入此分支。若物理 slot 仍在当前 UID 名下，沿用原 owner map 校验；若 slot 已由真实 deq 释放，则 `lsq_deq=1` 是唯一允许的资源状态，此时 status 保留的 LQ key 仅作为该 UID 的历史身份字段，不能再用 live map 反查。这样新 UID 即使已经复用同一 LQ key，也不会被旧 WB 错误归属，因为旧 WB 的 UID 仍由完整 ROB 和当前 issue epoch 决定。

### 3.2 复杂度与状态约束

该调整只在每个 raw Load WB 的现有 O(1) snapshot 回填路径增加一个布尔状态分支；不新增 map、queue、tombstone 或 UID 全表扫描。`uid_by_lq` 的插入/释放时机保持不变，避免破坏 LSQ capacity、deq pointer 和后续资源复用。

## 4. 验证与验收

1. 对同一 `pbmt_dynamic` real-DUT smoke 重新编译并运行。
2. 日志必须不再出现 `INT_WB_ATTACH LOAD uid=0 has no active LQ mapping`。
3. 观察到 deq 后 WB 时，WB event 仍有 `has_lq=1`，且 UID 由原有 ROB/epoch 检查归属。
4. PBMTE A/B/C 场景继续执行，检查 B 的 S1 PF、C 不继承 B 的 token-local fault，以及 `UVM_ERROR=0`、`UVM_FATAL=0`。
5. PBMT 动态 smoke 通过后，运行 `tc_sanity/base_fun` 基础回归。

## 5. 文档同步与归档

实现后生成 implementation review，覆盖 adapter 的修改前后逻辑、上述失败时序和验证结果。PBMT 两个专项 plan 仍按各自完成条件归档；本 plan 仅在本身实现、review 和必要仿真完成后从 `undo` 移至 `do`。

## 执行中补充/修正（IMPLEMENTATION_DELTA）

`[IMPLEMENTATION_DELTA]`

- 来源：首次 adapter 修复后的 real-DUT 复跑。
- 原 plan：只修改 `dispatch_monitor_event_adapter::fill_current_issue_snapshot()` 的 Load 分支。
- 新证据：adapter 已成功生成带 UID、ROB 和历史 LQ key 的 event，但
  `dispatch_monitor_batch_handler::normalize_event_batch()` 随后调用
  `common_data_transaction::normalize_feedback_event()`；后者再次进入
  `resolve_uid_for_event()`，无条件用 `has_lq` 查询 `uid_by_lq`，在 827.8ns 将事件丢弃。
- 实现调整：同一身份语义必须在公共 resolver 收敛，不允许 batch handler 建立 bypass。
- 影响范围：新增 `common_data_transaction::resolve_uid_for_event()` 的已 deq Load 历史 LQ identity 分支；adapter 的第一层检查保持不变。

### `resolve_uid_for_event()` 的补充 Flow

抽象功能描述：该函数归一化来自 UID、ROB、LQ 和 SQ 的 event identity。它优先保证多个仍有效的 live owner map 指向同一 UID；对于已经由 UID/ROB 唯一确认的 late Load WB，允许 LQ key 只作为历史 metadata，而不把已释放或复用的物理 LQ slot 反解成另一个 UID。

源码级伪代码：

```text
解析 UID 和 ROB 后处理 has_lq：
  若已有 uid，target 是 LOAD，且该 uid 的 status 满足：
    active_lq_mapped=0、lsq_deq=1、status 的 LQ key 等于 event LQ key：
      把 event LQ 当作历史 identity；不查询 uid_by_lq。
  否则：
      保持既有 lookup_active_uid_by_lq() 和 owner 一致性检查。
```

中文文字伪代码：resolver 先保留原有 `has_uid` 与完整 ROB key 交叉校验，确保候选仍是 active UID。随后若该候选是已经 deq 的 Load，event 中的 LQ key 与该 UID 冻结 key 相同，则不再读取 `uid_by_lq`；这个 map 的当前 owner 可以为空或属于后续 UID，均不能推翻已经确定的 WB 身份。其余事件，包括 LQ-only event、未 deq Load、Store/SQ event 和 LQ key 不一致事件，继续使用原有 live owner map，因此不会扩大对 resource lifecycle 的放宽范围。
