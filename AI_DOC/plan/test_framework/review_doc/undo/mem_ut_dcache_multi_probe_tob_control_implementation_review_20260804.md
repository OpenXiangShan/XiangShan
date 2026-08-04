# DCache 多 Probe、`Probe(toB)` 与轻量 L2 flush 实现 Review

| 项目 | 内容 |
|---|---|
| 关联 plan | `AI_DOC/plan/test_framework/plan/do/mem_ut_dcache_multi_probe_tob_control_plan_20260730.md` |
| 目标版本 | V2 |
| Review 范围 | DCache responder Probe policy、flush sideband、公共参数链和相关文档 |
| Review 结论 | 通过：实现复用既有 line/probe token owner；未发现遗漏的字段、状态清理或主流程所有权冲突 |
| 未覆盖边界 | CBO Probe deferred closure、完整 CoupledL2 directory/set/way 扫描、专用 flush directed vseq |

## 1. 术语与抽象功能说明

| 名词 | 当前实现中的含义 | 代码落点 | 生命周期 |
|---|---|---|---|
| `probe batch` | 一次随机 start 选择的一组互不重复 DCache line | `dcache_probe_record_t::batch_id` | record 创建时赋值；全部 C reply 收敛后自然结束 |
| `Probe record` | 一笔 B Probe 的唯一 B/C 生命周期 owner | `probe_record_q` | `submit_probe()` 建立；合法 C reply 后删除 |
| `B hold` | 当前正在 B channel 等待 `ready` 的单笔 Probe | `probe_b_hold_token` | `service_probe_b_hold()` 选中；B.fire 后转 WAIT_C |
| `flush snapshot` | DRAIN 收敛后冻结的 ACTIVE physical line 集合 | `l2_flush_snapshot_line_q` | DRAIN->PROBE 建立；本轮 flush 完成或 request 撤销后清理 |
| `flush state` | 轻量 L2 flush 的 request/done level 状态机 | `l2_flush_state` | `IDLE -> DRAIN -> PROBE -> DONE -> IDLE` |
| `direct VIF sample` | responder 从已有 other_ctrl interface 读取同拍 flush request | `other_ctrl_vif.mon_cb.io_outer_l2_flush_en` | 每个 DCache `drv_cb` 读取；只读不驱动 |

## 2. Review 范围

本轮修改覆盖：

- `env/plus.sv`、`seq_csr_common.sv` 和两个 cfg 中的 Probe 参数迁移；
- `dcache_mem__access_base_sequence` 的 batch 选择、shared Probe record 和 flush state；
- `dcache_agent_agent_driver::check_l2_sideband_item()` 对合法 DONE level 的 known-value 校验；
- DCache flow/source/interface、参数迁移、历史 plan/review 和 TODO 文档同步。

不改变：主表、status、LSQ、issue、writeback、commit/deq、pass/fail、terminal、shared memory 数据 key 或
Uncache responder 的正常调度。

## 3. 参数迁移 Review

抽象功能描述：公共参数链将旧的单概率 Probe gate 替换为随机 batch 的独立开关、开始概率、数量类别和
target 选择；所有 consumer 只通过 `seq_csr_common` getter 读取已校验结果。

文字伪代码：

```text
plus.sv：解析 EN、PRE_START、ONE/MID/LARGE、TO_B；不再定义旧 ENABLE_WT。
seq_csr_common：
  读取 plus 值；
  检查 PRE_START/TO_B 位于 0..10000；
  检查 ONE/MID/LARGE 至少一个非零；
  提供 getter。
dcache sequence：
  只通过 getter 随机开始、数量和 target；
  不保存第二份 runtime 参数状态。
```

正确性检查：`MEMBLOCK_L2_PROBE_ENABLE_WT` 已从 `plus.sv`、`seq_csr_common.sv`、active cfg 和 source
consumer 删除。`MEMBLOCK_L2_PROBE_EN=0` 默认关闭随机激励，保持普通 smoke 行为稳定。

## 4. 随机 Probe Batch Review

### 4.1 `sample_probe_batch_start()`、`sample_probe_batch_count()` 与 `sample_probe_target_cap()`

抽象功能描述：三个 helper 只完成一次随机选择，分别决定是否开始 batch、batch 数量和单条 target；不建立
Probe record、不改变 line 状态、不驱动 B channel。

文字伪代码：

```text
若 EN=0 或 PRE_START=0：不开始 batch；
否则按 1 : 0 = PRE_START : (10000-PRE_START) 抽取开始结果；
开始后按 ONE/MID/LARGE 权重抽取 1、2..6 或 7..15；
对 batch 内每条 line 按 TO_B : TO_N = TO_B : (10000-TO_B) 选择 target_cap。
```

检查结果：权重抽样均使用标准 SystemVerilog `std::randomize`，两个 10000 量级参数在 getter 前已被
`seq_csr_common` fail-fast 校验；不会产生超过 15 条的随机 batch。

### 4.2 `try_start_probe()`

抽象功能描述：该函数是随机 policy 的唯一创建入口。它只在 responder 没有任何既有 Probe、D response、
GrantAck 或 C assembly owner 的空闲窗口建立一个 batch；真正的 B/C 生命周期仍交给原有 shared service。

文字伪代码：

```text
若不允许随机、已有 response/Probe/C owner 或已 global stop：返回；
若未命中 batch start：返回；
分配新的 batch_id；
重复到目标数、无可选 ACTIVE line 或固定 16 条 capacity：
  从 cached_line_by_addr 选择无 pending Probe 的 ACTIVE line；
  调用 submit_probe(line, target_cap, RANDOM, batch_id)；
  submit_probe 立即将 line 转为 PROBE_PENDING；
若至少建立一条 record：推进 batch_id，并调用 service_probe_b_hold。
```

检查结果：同一 line 在 `submit_probe()` 后不再满足 `select_random_cached_line()` 条件，因此同 batch 不会重复。
队列满时循环自然停止，不会覆盖已有 record 或产生 fatal。`toB` 完成后回到 ACTIVE，`toN` 完成后删除 line，
均复用 alias foundation 的 `complete_probe_record()`。

## 5. L2 Flush Review

### 5.1 `service_l2_flush()`

抽象功能描述：该函数是轻量 flush 的唯一状态 owner。它读取 DUT level request，冻结新的 A request，
等待既有 D/E/B/C owner 自然收敛，然后只对冻结 snapshot 发 `Probe(toN)`；它不清 shared memory、
不取消 active transaction，也不写 dispatch 状态。

文字伪代码：

```text
IDLE 且 flush_en=1：进入 DRAIN；
DRAIN：
  若 request 提前撤销：fatal；
  等待 D queue/timer、GrantAck、Hint、B hold、Probe record、C assembly、armed A/C 全部为空；
  扫描 ACTIVE line 建立固定 snapshot；进入 PROBE；
PROBE：
  若 request 提前撤销：fatal；
  每轮最多从 snapshot 取一条 line，调用 submit_probe(line, toN, FLUSH, 0)；
  snapshot 空且所有 FLUSH Probe C 收敛：进入 DONE；
DONE：
  done=1；继续允许普通 A 接收，但暂停随机 Probe；
  观察 request=0：只清 snapshot 和 flush state，回到 IDLE。
```

检查结果：DRAIN/PROBE 通过既有 cycle item 的 A.ready=0 实际反压 DUT，而不是仅软件忽略请求。C channel
继续按已有 owner 接收，因此不会将正在返回的 `ProbeAckData` 或 `ReleaseData` 卡住。DONE 只由 state 驱动，
driver 只允许已知 0/1，不会将合法 done level 误判为非法。

### 5.2 `body()` 的 flush 接入

抽象功能描述：`body()` 在既有 A/B/C/D/E fire 结算之后、下一拍 A.ready 仲裁之前调用 flush owner，
保证 request 到来前已真实 accepted 的请求自然 drain，而 request 后不再新建 A response owner。

文字伪代码：

```text
每个 drv_cb：
  从 other_ctrl VIF 读取 flush_en，并检查非 reset 时为 0/1；
  结算上一 item 的 D/E/C/B/A fire；
  调用 service_l2_flush(flush_en)；
  DRAIN/PROBE 时 l2_flush_blocks_a_request 返回 1，跳过 A.ready；
  IDLE 才允许 try_start_probe；DONE 输出 io_l2_flush_done=1。
global stop：只有 flush state 已回到 IDLE 且 request=0 时才能自然退出。
```

## 6. 与 Plan 对齐

| Plan 项 | 实现判断 | 说明 |
|---|---|---|
| 16 条固定 Probe capacity | 一致 | 继续复用 alias foundation 的 `DCACHE_MAX_PROBE_RECORDS=16` |
| 随机 1/2..6/7..15 batch | 一致 | 由三个 count weight 选择，line 立即 record 化去重 |
| 随机 toB/toN | 一致 | 由 `MEMBLOCK_L2_PROBE_TO_B_WT` 决定，C reply 沿用 target_cap 收敛 |
| l2Flush DRAIN/snapshot/DONE | 一致 | 新 A 反压、C/B/D/E 自然 drain、done 保持到 request 撤销 |
| 不实现完整 L2 | 一致 | 不新增 directory、MSHR、set/way、其它 client 或 CBO closure |

## 7. 与原 Plan 不一致的实现

### 同拍 `other_ctrl` VIF 采样

Plan 原文将 `io_outer_l2_flush_en` 描述为由 `other_ctrl_agent` monitor 观察。当前 monitor 没有为 responder
发布可消费的 raw item；若新增上一拍共享 level，会多接受一拍 A request。实现因此直接取得既有
`other_ctrl_agent_agent_interface` 的 read-only VIF，同拍采样 request。该调整已写入 plan 的
`IMPLEMENTATION_DELTA`，没有新增 agent、接口端口、raw queue 或第二份状态。

### 随机开关不阻断功能性 flush

Plan 中“EN 关闭不产生 Probe”的字面范围易误导。实现将 EN 限定为随机 policy：DUT 已发起的 flush 必须继续
提交固定 toN Probe，否则无法产生 done。该职责修正已写入 plan `IMPLEMENTATION_DELTA`。

## 8. 验证与剩余风险

已完成：

```text
make eda_compile tc=basicTest ts=virtual_base_sequence \
  mode=dcache_probe_flush_20260804 partcmp_op=off

make eda_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
  mode=dcache_probe_flush_20260804 cfg=tc_dispatch_real_smoke partcmp_op=off
```

结果：VCS 编译 `0 error(s), 0 warning(s)`；真实 smoke `TEST_PASS`，`UVM_ERROR=0`、`UVM_FATAL=0`。

本轮未新增专用 flush directed vseq，因此随机 batch/toB 和 CSR 触发的 flush snapshot 尚未用单独 testcase
逐条覆盖；代码路径已通过编译和现有真实 DCache main flow 回归。CBO Probe deferred closure 仍由
`mem_ut_v2_dcache_cbo_probe_closure_plan_20260731.md` 独立实现，不能在本 review 中视为已完成。

## 9. 最终结论

Review 通过。实现只在 DCache responder 的既有 line/probe lifecycle 上增加参数化 batch policy 和 flush-local
状态机；没有改变测试框架主表控制逻辑，也没有引入第二份 Probe、alias、memory 或 completion owner。
