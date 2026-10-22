# STA coalesced feedback replay 生命周期修复计划（2026-10-19）

| 项目 | 内容 |
| --- | --- |
| 状态 | 独立 review、coding、专项测试及失败 seed复测通过；100-seed回归进行中 |
| 版本 | V2，`mem_ut_uvm_v2` |
| 关联缺陷 | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/AI_DOC/buglist/rm/v2/rm_buglist_2026-W43_20261019.md` |
| 修改范围 | STA failed feedback 到 replay recovery queue 的生命周期；定向 replay smoke |
| 非目标 | DUT RTL、Scala、feedback 接口定义、fault writer、cfg权重、PTW/redirect仲裁策略 |

## 专有名词与抽象功能说明

| 术语 | 中文含义 | 代码落点 |
| --- | --- | --- |
| tombstone | 为迟到 STA fault保存的旧 issue身份锚点。 | `sta_late_fault_tombstone_q` |
| coalescing | 同一 dynamic epoch、ROB、SQ只保留最早 tombstone。 | `capture_sta_late_fault_tombstone()` |
| recovery event | 驱动 replay状态推进的 feedback queue事件。 | `push_feedback_event()` / `handle_replay_event()` |
| current identity | 当前有效的 `issue_epoch + replay_seq`。 | `status_transaction` |

抽象功能描述：本方案恢复 history owner和 replay owner的职责分离。tombstone helper只决定
history是否新增；writeback handler负责把每条有效 failed feedback交给既有 recovery owner。

## 根因与目标 Flow

修改前：

```text
STA failed -> capture返回成功但created_new=0 -> caller提前返回 -> recovery event丢失
```

修改后：

```text
STA failed
  -> capture/coalesce tombstone
  -> capture失败：按stale丢弃
  -> capture成功：created_new只选择日志
  -> 总是push当前feedback event
  -> recovery handler按PTW/redirect仲裁调用mark_replay_pending()
```

## 源码修改 Flow

### `writeback_status_handler::handle_issue_feedback_event()`

抽象功能描述：校验并登记 IQ feedback；对 STA failed只建立 history并排队 recovery，不直接
修改 replay状态。

详细文字伪代码：

```text
if feedback is current STA failed:
  if capture_sta_late_fault_tombstone() fails:
    return stale
  if created_new:
    log capture
  else:
    log coalesce
  push_feedback_event(feedback)
  return handled
```

保持边界：STD feedback规则、STA hit成功、real writeback、fault落表、PTW wait和redirect-first
优先级不变。

### `soft_test_memblock_dispatch_replay_smoke_sequence`

抽象功能描述：软件定向验证 replay queue、identity guard和 tombstone history在重复及跨轮
failed场景下保持一致。

验证步骤：

1. 同一 current identity连续注入两条 failed，再统一处理 recovery；确认只推进一次 replay。
2. 检查 tombstone history长度为1，且保留首次 issue/replay identity。
3. 重新发射后注入第二轮 failed；确认再次推进 replay，但 history仍为1。
4. 第二次重新发射后先注入 IQ hit，再验证两轮旧 writeback均被丢弃，current writeback正常完成。

## 正确性边界

1. 完全重复 recovery event不会重复重发：第一条更新 identity后，第二条由
   `mark_replay_pending()` 的 current identity guard返回0。
2. 同 stable scope不同 replay sequence仍表示新的当前失败，不能被 history coalescing吞掉。
3. capture失败的 stale feedback不进入 recovery。
4. fault/terminal先落表时，已排队 replay由 active/fault/terminal guard丢弃。
5. redirect/reset/retire继续使用既有 cleanup，不跨 dynamic instance复用 tombstone。
6. 不伪造 pass、不限制 replay次数、不降低异常频率、不修改 DUT激励。

## 验证计划

1. `git diff --check`和定向源码 review。
2. 编译新 mode并运行 `tc_dispatch_replay_smoke`，要求自然 `$finish`、`UVM_ERROR=0`、`UVM_FATAL=0`。
3. 原 cfg、原请求数重跑 `seed=1646764510`，要求 `RM_LS_TRACE_COMPARE=10000`、`TEST_PASS`。
4. 恢复问题3配置100-seed回归；所有 seed必须完整执行10000请求并满足六项通过标准。
5. 若出现新的首个错误，按 bug规则单独分类，不扩大本修复吞掉异常。

## 验证进展（2026-10-19）

1. `git diff --check`通过；新 mode `fault3_sta_replay_lifecycle_fix_20261019`编译成功。
2. `tc_dispatch_replay_smoke / virtual_base_sequence / seed=666666`通过，重复 feedback只推进一次，
   第二轮 coalesced feedback继续推进到 `replay_seq=2`，正常 `$finish @ 1265.300ns`。
3. 原失败 `seed=1646764510`按原 cfg完整执行10000请求并通过：
   `RM_LS_TRACE_COMPARE=10000`、`UVM_ERROR=0`、`UVM_FATAL=0`、`Quit count=0/10`、
   正常 `$finish @ 2421373.000ns`。
4. 原批次其余7个已启动 seed全部通过；剩余92个 seed正使用修复后的同一 binary续跑。
