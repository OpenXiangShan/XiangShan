# MemBlock Load MMIO Redirect R+2 旧 Owner 修复 Plan Review

评审日期：2026-10-12

评审对象：`AI_DOC/plan/test_framework/plan/do/memblock_load_mmio_redirect_r2_stale_owner_fix_plan_20261012.md`

评审结论：通过，无 coding blocker。

## 1. 术语与抽象功能说明

| 英文术语 | 当前评审中的中文含义 | 对应代码对象或落点 | 示例 |
|---|---|---|---|
| `active map` | 当前仍有效动态实例的 ROB 快速反查表 | `uid_by_active_rob` | 正常 `loadMmio` 只 probe flag 0/1 两个完整 key |
| `slow path` | 仅 R+2 且 active owner 缺失时执行的历史证明扫描 | 计划新增 helper | 正常 event 不全表扫描 |
| `redirect-deleted owner` | redirect 删除 active map 前保存的单代旧 LQ/SQ 身份 | `redirect_deleted_lq_owner_by_key` | 保存 uid、旧 dynamic epoch、redirect epoch和record id |
| `stale-drop` | 严格证明为旧流水线残留后，只丢 raw、不写 MMIO tag | `MEMBLOCK_MMIO_RESOLVE_STALE_DROP` | 问题2的 R+2旧 Load pulse |
| `wrap ambiguity` | raw 只有 ROB value，flag 0/1 都存在兼容候选 | `memblock_rob_key_t.flag` | 两个历史候选时必须 fatal |

抽象功能评审：本 plan 的 resolver 修改只负责 owner 解析和 stale-drop判定，不承担 redirect状态推进、LSQ资源释放或 MMIO tag创建；定向 sequence只构造软件场景，不进入真实 DUT flow。职责边界正确。

## 2. 内容完整性评审

Plan 已覆盖用户要求的全部核心约束：

- 不修改 RTL。
- 不修改 monitor 的 `mmio_flush_epoch`。
- 不全局忽略 `loadMmio`。
- 正常 active owner仍设置 MMIO tag。
- Load overlap上界只扩到 R+2。
- R+2 不能仅凭 sample offset丢弃。
- R+3、无历史 owner、多候选、新 active owner和非 Load保持 fatal。
- 定向软测试、真实 seed和正常 smoke均有明确验收标准。
- 保留前序 `common_data_transaction.sv` 与 `dispatch_monitor_event_adapter.sv` 诊断改动。

未发现遗漏的参数、cfg、interface或 filelist工作；本专项无需新增这些对象。

## 3. 正确性评审

### 3.1 status-only 方案评审

当前 `prepare_uid_for_redirect_reissue()` 会通过 `clear_uid_dispatch_result()` 清除 `load_dispatched`、active activation epoch和 target flush epoch。若仅依据 redirect 后 status中的 `flushed/issue_killed`，无法证明候选由当前 overlap redirect删除，可能把更早历史误认为本次旧 owner。

Plan 已把该风险转化为正式约束：status/main table负责恢复静态 Load类型、完整 ROB key、历史 issue和当前 inactive状态；现有 `redirect_deleted_lq_owner_by_key` 负责绑定 uid、旧 dynamic epoch、redirect epoch和 cancel record id。该组合能够区分当前 redirect旧实例与更早历史，无需新建 map。修正合理。

### 3.2 active owner优先级

Plan 明确先 probe active map，再考虑 R+2 slow path；且 slow path只在 `active_candidate_count==0` 时执行。这保证同 ROB value被 redirect后新实例复用时不会被旧历史吞掉。通过。

### 3.3 wrap与唯一性

Plan要求保存并比较完整 ROB key，同时候选数必须恰好为1。flag 0/1 都能通过时 fatal，符合 value-only接口的安全边界。通过。

### 3.4 时序边界

窗口固定为 `R..R+2`，只对 Load生效。R+3不进入 overlap slow path，能在未来 RTL时序变化时显式暴露接口合同变化，而不是静默吞掉。通过。

## 4. 测试框架整体影响评审

本修改不改变 MMIO tag setter/query、pending-MMIO sideband、commit/deq、redirect/cancel状态机和 monitor raw格式。正常路径仍是两个 active ROB key probe；主表扫描只发生在异常 R+2且 active owner为零的低频路径。符合高频路径禁止全表扫描规则。

现有 redirect-deleted owner窗口为单代且有固定清理边界，R+2 helper只读取该窗口，不延长生命周期、不增加第二 owner真源。通过。

## 5. 定向测试可执行性评审

现有 software-only sequence已经具备 main table构造、issue fire、redirect anchor、expected-fatal catcher和 monitor adapter入口。新增 R+2场景可以继续复用这些对象。

为了得到真实 redirect-deleted owner窗口，coding时测试 helper必须按公开生命周期执行：

1. 为已 admission Load建立并完成 LSQ reservation sample，使 cancel scan合法。
2. 创建 redirect record并绑定 anchor。
3. 记录 redirect drive完成和 R+1 cancel snapshot watermark。
4. 调用正式 redirect flush入口，让 `capture_redirect_deleted_owner()`、active map删除和窗口启动按生产代码执行。
5. 再注入 R+2或 negative raw。

不得在 soft-test中直接写 `status.active`、`uid_by_active_rob`、deleted-owner map或 cancel record字段来伪造成功状态。该执行约束应在 coding中落实。

## 6. 性能评审

| 路径 | 修改后复杂度 | 结论 |
|---|---|---|
| 正常 active MMIO raw | 两个完整 ROB key probe | 不变 |
| R/R+1 overlap | 有界 cancel record/anchor扫描加两个 active key probe | 不变 |
| R+2且无 active owner | 一次主表/status扫描，再查询现有 LQ owner map | 允许的异常 slow path |
| fatal诊断 | 既有全表 dump | 低频 debug路径 |

未发现每拍或每普通 monitor event新增全表扫描。

## 7. 风险与非 blocker

1. `load_issue_epoch` 是旧实例曾真实 issue的历史证据，但不单独表达其 dynamic epoch；必须和 redirect-deleted owner的 `old_dynamic_epoch + 1 == status.dynamic_epoch` 联合使用。Plan已明确。
2. 真实 seed可能在问题2消失后继续暴露问题1或问题3。测试报告必须按错误 ID和时间区分，不以整条 seed是否最终 pass作为唯一问题2判据。Plan已明确。
3. 前序诊断打印较多，但只在最终 fatal路径执行，不污染高频成功路径。可保留。

## 8. 最终结论

正式 plan 已把 status-only方案的证明缺口修正为“status/main table + 现有 redirect-deleted LQ owner”的联合证明；没有新增长期状态，没有扩大 Store行为，也没有把 R+2本身作为充分条件。测试方案可通过生产生命周期 API构造，不需要直接篡改公共 owner状态。

评审结果：无 blocker，可以按该 plan coding。
