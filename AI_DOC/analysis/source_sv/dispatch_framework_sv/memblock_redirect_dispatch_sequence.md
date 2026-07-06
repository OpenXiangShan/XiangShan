# memblock_redirect_dispatch_sequence.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_redirect_dispatch_sequence.sv`

## 1. 文件定位与使用场景

该 sequence 是 dispatch real flow 的 redirect 接口回灌者，挂在
`env.u_redirect_agent_agent.sqr.main_phase`。它不自己判断是否需要 redirect，也不直接
flush 软件状态表；它只消费 `common_data_transaction` 中由
`exception_redirect_replay_handler` 投递的 `pending_redirect_drive_q`，构造
`redirect_agent_agent_xaction`，把 redirect pulse 真实驱动到 DUT `io.redirect`。

为什么需要它：

- `memoryViolation` 等 DUT output ctrl event 先由 monitor 采集到软件状态机。
- 只在 TB 内部调用 `apply_redirect_flush()` 会让 TB 以为已经 flush，但 DUT 没收到
  `io.redirect`，两边状态可能分叉。
- 该 sequence 把公共恢复逻辑选出的 redirect payload 发回 DUT，形成后端接口闭环。

## 2. 调度与状态关系

| 阶段 | 条件 | 动作 |
|---|---|---|
| 初始化 | main phase default sequence 启动 | 调用 `seq_csr_common::init()`，读取 `MEMBLOCK_REDIRECT_SEQ_EN` 和 timeout。 |
| 空闲等待 | redirect queue 为空 | 每拍发 idle xaction，保持 `io_redirect_valid=0`。 |
| 取 payload | `try_pop_redirect_drive()` 成功 | 标记 redirect inflight，构造真实 redirect xaction。 |
| 驱动 DUT | payload valid | `io_redirect_valid=1`，`robIdx/level` 来自 payload；payload 来自 `memoryViolation.bits.robIdx/level` 等 DUT output ctrl 事件。 |
| 完成通知 | xaction 完成 | 调用 `mark_redirect_drive_done(payload)`，允许 recovery handler 进入 state flush。 |

该 sequence 不持有 UVM objection。main phase 生命周期由 testcase/main sequence 管理，
避免常驻 responder 因自己 raise objection 导致仿真无法收尾。

## 3. 参数

| 参数 | 默认 | 作用 |
|---|---|---|
| `MEMBLOCK_REDIRECT_SEQ_EN` | `1` | 是否启用真实 redirect 回灌。关闭时只发一次 idle 后退出，且一旦 recovery handler 检测到 redirect/memoryViolation event 会 fatal；普通 real smoke 应保持开启。 |
| `MEMBLOCK_REDIRECT_DRIVE_TIMEOUT` | `1000` | active redirect 等待 drive 过久时 fatal，避免无声挂死。 |

## 4. 边界

- 本 sequence 只驱动当前 RTL 顶层已有的 `io.redirect` 字段，不模拟前端 refetch。
- `MEMBLOCK_REDIRECT_SEQ_EN=0` 只适合无 redirect event 的场景。关闭后若仍产生 redirect/memoryViolation，公共 recovery 会 fatal，避免 freeze 后 payload 永远无人驱动。
- 当前 RTL 顶层没有单独 `flushItself` 端口；`RedirectLevel.flushItself(level)` 等价于 `level(0)`。TB payload 中保留 `flush_itself` 只用于软件 flush 范围。
- `io_redirect_*` 是本 sequence/driver 送入 DUT 的 input 接口；redirect monitor 不再反馈 dispatch recovery，旧 Self Redirect Filter 已删除/停用。
- flush 范围和状态清理由 `common_data_transaction::apply_redirect_flush()` 统一完成。
- admission/issue/commit 冻结由 `issue_blocked_by_global_flush()` 协同完成，不在本 sequence 内私自改其它队列。
- `MEMBLOCK_REDIRECT_DRIVE_TIMEOUT` 是本 sequence 的 idle loop 计数；真正 redirect freeze 等待 timeout 使用 dispatch service-cycle。
