# MemBlock 真实 issue-fire 边界记账修复方案（2026-08-28）

| 项目 | 内容 |
| --- | --- |
| 状态 | 可执行，等待本轮 coding 与远端验证 |
| 适用版本 | V2，分支 `mem_ut_uvm_v2` |
| 关联失败 | `INT_WB_STD_KEY`，`rm_sv39_10k_std_diag_20260828`，`397.800ns` |
| 关联分析 | `AI_DOC/analysis/framework_design/memblock_rm_std_value_only_unowned_writeback_analysis_20260828.md` |
| 修改边界 | UVM xaction、lintsissue driver、issue sequence；不修改 RTL、RM algorithm 或 DUT interface |
| 目标场景 | Sv39、U 态、`MEMBLOCK_MAIN_TRANS_NUM=10000` 的 real-dispatch 回归 |

## 1. 专有名词与抽象功能说明

| 术语 | 当前含义 | 代码落点 | 示例 |
| --- | --- | --- |
| 真实握手（fire） | 某个 scalar issue port 在同一个 driver clock sample 满足 `valid && ready`；这是 DUT 接收该 payload 的唯一判据。 | `lintsissue_agent_agent_driver::clear_ready_dispatch_issue_ports()` | `issueStd_0_valid=1` 且 `issueStd_0_ready=1`。 |
| candidate descriptor | sequence 选择并填入 xaction 的 `memblock_issue_q_item_t`；其中有 UID、target、ROB/SQ key、replay sequence 和 pipe index。 | `memblock_issue_dispatch_base_sequence::assign_issue_items()` | UID 57 的 STD candidate 占用 `std_port=0`。 |
| fire 状态 | driver 在真实握手点置位的 `fired_mask` bit；它在当前 xaction 生命周期内保持，直到 sequence 完成该 item。 | `lintsissue_agent_agent_xaction::memblock_dispatch_fired_mask` | STD port 0 fire 后，对应 bit 保持为 1，watcher 可以可靠观察。 |
| fire 记账 | 用 candidate descriptor 更新 issue epoch、target instance flush epoch、issue queue 与 `*_dispatched`。 | `issue_queue_scheduler::mark_issue_fire()` | STD fire 后 `std_dispatched=1`、`std_issue_epoch!=0`。 |
| 已记账 mask | 当前 xaction 中已经由 watcher 处理的 scalar port；仅用于阻止 `finish_item()` 后重复记账。 | `send_issue_cycle()` 的本地 `bookkept_fired_mask` | port 5 已处理后不再分配第二个 issue epoch。 |
| redirect 接受路径 | fire 与 flush/redirect 同拍时，DUT 已接收的 payload 仍需按既有“已接受”规则做一次框架记账。 | `issue_queue_scheduler::mark_issue_fire_already_accepted()` | driver 先看到 ready、随后检测到 flush epoch 改变。 |

抽象功能描述：本 plan 将 fire 记账从 `finish_item()` 返回后的延迟阶段移动到 driver 已确认
`valid && ready` 的同一 UVM 时间边界。driver 只置位持久 fire 状态，不直接写公共状态；sequence 仍是
candidate descriptor 与 `issue_queue_scheduler` 的所有者，因此 UID、replay、queue 和 epoch 的语义不分裂。

## 2. 问题与目标 Flow

当前失败路径：

```text
sequence 构造 candidate
  -> driver 驱动 valid
  -> DUT/monitor 已产生 STD writeback
  -> driver 观察 ready，只回填 fired_mask
  -> main service 消费 writeback
  -> adapter 要求 std_dispatched=1，但 finish_item 尚未返回，状态仍为 0
```

修复后的路径：

```text
sequence 构造 candidate 并启动持久状态 watcher
  -> driver 驱动 valid
  -> driver 在 valid&&ready 置 fired_mask
  -> watcher 观察未记账 fired bit，用同一 candidate 调用 scheduler 完成一次 fire 记账
  -> main service 消费 writeback，看到完整 issue snapshot
  -> finish_item 返回后仅补偿未记账的 fired bit
```

目标约束：

1. `std_dispatched`、`std_issue_epoch` 和 target instance flush epoch 必须在 main service 消费同次
   writeback 前完成建立。
2. driver 不得引用或修改 `common_data_transaction`、issue queue 或 UID map。
3. sequence 不得根据 `fired_mask` 重复执行 `mark_issue_fire()`。
4. flush/redirect、blocking/nonblocking issue、load/STA/STD 所有 port 保持原有握手定义。
5. 运行期只处理当拍候选集合，不能扫描 10000 笔 main table。

## 3. 字段与职责边界

### 3.1 `lintsissue_agent_agent_xaction::memblock_dispatch_fired_mask`

抽象功能描述：该字段由 driver 在真实握手点置位，表示当前 xaction 内已经被 DUT 接收的 scalar port。
sequence watcher 以它作为持久条件完成记账；它不表达 DUT payload，也不进入 monitor/RM 的 transaction 比较。

| 项目 | 规则 |
| --- | --- |
| 写者 | `record_dispatch_issue_fire()`，且仅在 port 的 `valid && ready` 成立后置对应 bit。 |
| 等待者 | `send_issue_cycle()` 在交付 item 前启动的 watcher。 |
| 清理 | xaction 初始化和每次 driver 发射开始时清零；xaction 生命周期结束自然释放。 |
| 不变量 | 已置位 bit 不会因 watcher 消费而清零；`bookkept_fired_mask` 单独记录已记账状态。 |

### 3.2 `bookkept_fired_mask`

抽象功能描述：该局部 mask 记录本次 xaction 已通过 watcher 完成的 port，防止 `finish_item()` 返回时对
同一 candidate 再次分配 issue epoch 或重复删除 queue entry。

| 项目 | 规则 |
| --- | --- |
| 写者 | `mark_fired_items()` 在处理实际 fired port 后写入。 |
| 读者 | `send_issue_cycle()` 的 watcher 与 finish 后补偿路径。 |
| 生命周期 | 仅限一次 `send_issue_cycle()` 调用，不保存到 status table。 |
| 恢复 | redirect/flush 后随该 xaction 返回自然失效；全局恢复仍由既有 handler 负责。 |

## 4. 主流程实现

### 4.1 `lintsissue_agent_agent_driver::record_dispatch_issue_fire()`

抽象功能描述：该函数是唯一确认 scalar port 真正被 DUT 接收的 driver 边界。它置位对应的持久
port bit 并保留诊断日志；不解释 UID、不会出队、不会更新 status。

详细文字伪代码：

1. 检查 xaction 句柄和 global port index 合法，非法输入直接 `uvm_fatal`。
2. 将对应 `memblock_dispatch_fired_mask` 置 1，保留既有 sequence 侧 fire 结果接口。
3. 保留当前 driver 日志；日志是诊断信息，不是状态真源。

### 4.2 `memblock_issue_dispatch_base_sequence::send_issue_cycle()`

抽象功能描述：该 task 在本轮 candidate 已冻结后，将 item 交给 driver，并在 driver 真实 fire 时立即完成
对应 framework 记账。`finish_item()` 仍只用于确认 driver 已结束该 xaction；它不再是唯一的记账起点。

详细文字伪代码：

1. 创建并清空 xaction，明确设置 `pre_pkt_gap=0`、`post_pkt_gap=0`，防止周期 item 引入无关随机延迟。
2. 选择 load/STA/STD candidate，并把每个 candidate 及其实际 port index 固定在 `fired_items` 中。
3. 在 `start_item()` 成功后，启动 watcher；watcher 等待
   `tr.memblock_dispatch_fired_mask & candidate_mask & ~bookkept_fired_mask` 非零。该条件是持久状态，
   即使 driver 在 watcher 实际阻塞前已置位，也会立即返回，因此不存在瞬时 event 的 delta-cycle 丢失。
4. watcher 每次观察到新增 fire bit，都以
   `tr.memblock_dispatch_fired_mask & candidate_mask & ~bookkept_fired_mask` 找到新增 fire port；它调用 `mark_fired_items()`，该 helper 复用当前 target 的
   `mark_issue_fire()` 或 `mark_issue_fire_already_accepted()`，完成 status、issue snapshot 与 queue 更新。
5. `finish_item()` 返回后重新计算有效 fired mask；只对 watcher 尚未处理的 bit 做一次补偿，以覆盖 watcher
   与 item_done 同一 delta 的边界，不重复处理已记账 port。
6. 停止 watcher，再执行既有 blocking complete、redirect/flush cancel 和 has-fire 判断；未 fire candidate
   仍留在 queue，下一轮可重新仲裁。

### 4.3 `memblock_issue_dispatch_base_sequence::mark_fired_items()`

抽象功能描述：该 helper 接收当前 xaction 的有限 candidate 与新增 fired port mask，完成一次且仅一次
scheduler 记账。它不遍历 main table，只遍历 scalar issue port 数量上界的 candidate list。

详细文字伪代码：

1. 遍历本周期 candidate，计算每个 candidate 的 global port index。
2. 对不在新增 mask 或已经位于 `bookkept_fired_mask` 的 port 直接跳过。
3. 当前处于 global flush 时调用 `mark_issue_fire_already_accepted()`；否则调用
   `mark_issue_fire()`。两个现有 helper 分别处理一般 eligible 状态与同拍已接受的 flush 边界。
4. 无论 scheduler 成功还是报告 stale candidate，都把该真实 fired port 标记为已处理，避免同一 xaction
   在 watcher 和 finish 补偿两处重复告警或重复记账；失败仍保持现有 `uvm_warning` 以便定位。
5. scheduler 成功时，既有实现负责分配 issue epoch、调用 `mark_issue_snapshot()`、删除 queue entry、
   清 queued bit、置 dispatched bit 与清理 replay target。本 helper 不复制这些状态逻辑。

## 5. 失败策略与边界

| 场景 | 行为 |
| --- | --- |
| driver 返回 candidate 外的 fired bit | 保留既有 `uvm_fatal`。 |
| blocking item 没有完成所有 candidate fire | 保留既有 `uvm_fatal`。 |
| redirect/flush 同拍 fire | 走 `mark_issue_fire_already_accepted()`；不把已接收 port 静默留在 queue。 |
| 已被 recovery 淘汰的 stale candidate | 只报告一次既有 `uvm_warning`，不重建 UID map，不吸收未知 writeback。 |
| finish 之后出现未被 watcher 标记的 fire | 使用一次补偿路径；其作用仅覆盖 UVM delta 边界。 |

本 plan 不新增 plusarg、cfg、权重或 RTL signal；默认 testcase 行为仅在真实 issue handshake 的记账时刻提前。

## 执行中补充/修正（IMPLEMENTATION_DELTA）

`[IMPLEMENTATION_DELTA]`

来源：对 VCS 使用的 UVM 1.2 `uvm_event::wait_trigger()` 语义复核。

原 plan：使用 xaction 私有 `uvm_event`，driver 在 fire 后 `trigger()`，sequence watcher 使用 `wait_trigger()`。

实现调整：不新增 event 字段；watcher 直接等待既有 `memblock_dispatch_fired_mask` 中的“candidate 内且尚未记账”bit。

原因：UVM 1.2 明确规定 `wait_trigger()` 若在同一 delta 晚于 `trigger()` 调用会错过本次唤醒。用
`fired_mask` 的条件等待后，fire 事实在 xaction 生命周期内持续可见，先后调度顺序不会造成漏记账或
`finish_item()` 后永久等待。

影响范围：只影响本 plan 的 xaction/driver/event 方案，最终源码仅修改 issue sequence；真实握手判定、
scheduler 所有权、queue/epoch 更新和 RTL interface 均不变。验证项目保持不变。

## 6. 验证与验收

1. 对修改范围执行 `git diff --check`，检查持久 fire mask、helper 名称和中文注释。
2. 远端重新编译：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=<独立模式名> cfg=tc_dispatch_real_mmu_sv39_smoke
```

3. 使用 `wave=on` 运行固定 seed 的 10000 笔 Sv39/U 态场景，验收条件：

```text
TEST CASE PASSED
UVM_ERROR = 0
UVM_FATAL = 0
日志中不再出现 INT_WB_STD_KEY
```

4. 若仍出现 RM/framework 报错，记录新的日志与 FSDB，先判断能否由 framework 状态生命周期解释；能解释则新增
   分析和修复 plan 后继续测试。
5. 仅在 framework 已完成真实 fire 记账且波形仍显示无对应输入的 DUT 输出时，才启动独立 RTL review subagent。

## 7. 文档与提交

本轮 coding 后应同步：

- 本 plan 的执行状态与验证结果；
- `AI_DOC/analysis/framework_design/memblock_rm_std_value_only_unowned_writeback_analysis_20260828.md`；
- `AI_DOC/plan/test_framework/review_doc/undo/` 下的 implementation review。

通过静态检查、review 和基础仿真后，将 plan 移到 `AI_DOC/plan/test_framework/plan/do/`，并只提交本 feature
的 xaction、driver、sequence 与上述文档；不包含用户已有的无关工作区修改。
