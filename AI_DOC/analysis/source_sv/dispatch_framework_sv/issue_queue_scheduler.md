# `issue_queue_scheduler.sv` 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq_help/issue_queue_scheduler.sv`

## 1. 文件职责

`issue_queue_scheduler` 负责 scalar LOAD/STA/STD 的 route、候选选择和真实 fire 后的状态推进。
它不写 DUT payload；字段赋值由 `issue_field_assigner` 完成，valid/ready 采样由 lintsissue driver
完成。

主要数据结构是 `common_data_transaction` 中的三条轻量队列：

- `load_issue_q`
- `sta_issue_q`
- `std_issue_q`

queue item 保存 `uid/rob_key/target/send_pri/ready_cycle/replay_seq/lq_key/sq_key/uop_index` 等
发射所需元信息。完整 transaction 仍以 uid 从主表获取。

## 2. Route 与候选选择

`prepare_issue_route_for_uid()` 是 LSQ admission 后的直接入口：要求 uid 已 active/enq，置
`issue_ready=1`，再调用 `route_uid()`。`route_uid()` 复用
`lsq_ctrl_model::derive_op_behavior()`：load/prefetch 进入 LOAD，scalar store 同时进入 STA 和
STD。unsupported operation 最迟在统一 behavior 或字段赋值边界 fatal，不会被静默改成普通
scalar operation。

`route_all_ready_uids()` 是每拍补 route 路径。它只扫描公共 active window，且每拍最多扫描
`MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM` 个 uid；物理 slot 数来自 compile profile，不建立 runtime
硬件结构镜像。

`select_issue_candidates()` 分别使用 `MEMBLOCK_DUT_LOAD_PIPE_NUM`、
`MEMBLOCK_DUT_STA_PIPE_NUM`、`MEMBLOCK_DUT_STD_PIPE_NUM` 选择候选。原有 send-priority、ROB
年龄、delay 和 replay eligibility 规则保持不变。

## 3. Pipe 与 fired-mask 映射

`make_issue_item()` 把 `uop_index` 初始化为 0。真正构造本拍 xaction 时，
`memblock_issue_dispatch_base_sequence::assign_issue_items()` 将当前 target 候选数组下标写入
`fired_item.uop_index`。它表示该 item 本拍使用的 target-local pipe，不是主表 micro-op 编号。

映射规则：

| target | target-local `uop_index` | DUT 端口 | fired-mask bit |
|---|---:|---|---:|
| LOAD | `0..LOAD_PIPE_NUM-1` | `issueLda_<index>` | `LOAD_PORT_BASE + index` |
| STA | `0..STA_PIPE_NUM-1` | `issueSta_<index>` | `STA_PORT_BASE + index` |
| STD | `0..STD_PIPE_NUM-1` | `issueStd_<index>` | `STD_PORT_BASE + index` |

base、port count 和 mask width 均由 compile profile 派生。V2 默认仍是 3/2/2 和 7-bit mask，
但 scheduler/sequence/driver 不再把 `0/3/5`、`[6:0]` 或 `7'h7f` 当作第二权威。

## 4. Pending-work 查询

`has_pending_issue_work()` 是 issue 主循环的 O(1) 诊断入口，只读取三条 queue 的 `size()`：

```text
如果 load_issue_q 非空，返回 1；
否则如果 sta_issue_q 非空，返回 1；
否则如果 std_issue_q 非空，返回 1；
否则返回 0；
不扫描主表、状态表、active map 或 queue item 内容；
不修改 queue、status、counter 或 dispatch progress。
```

该函数不定义 sequence 正常退出。正常退出仍由 global stop/terminal 合同控制；它只用于区分
“queue 有待发工作但长期没有 fire”和“issue queue 已空，合法等待 writeback/commit/deq”。

## 5. 关键函数

| 函数 | 功能和副作用 |
|---|---|
| `make_issue_item(uid, target, behavior)` | 从主表/status 生成轻量 item；普通 scalar `uop_count=1`。 |
| `is_uid_route_ready(uid)` | 检查 active/enq/issue_ready、flush/redirect/exception/replay 条件。 |
| `route_target()` | 去重、删除 stale queue entry、压入目标队列并置 queued bit。 |
| `route_uid()` | 根据统一 behavior 调用 LOAD/STA/STD route。 |
| `route_all_ready_uids()` | 在公共 active window 内做 compile slot 上界的有限扫描。 |
| `has_pending_issue_work()` | O(1) 查询三条 issue queue 是否存在待发项。 |
| `advance_issue_queue_delays()` | 每拍递减 queue item 的 `ready_cycle`。 |
| `select_issue_candidates()` | 保持原有优先级和年龄规则，按 compile pipe 数选择三类候选。 |
| `mark_issue_fire()` | 普通路径分配 issue epoch、删除 queue item、清 queued、置 dispatched。 |
| `mark_issue_fire_already_accepted()` | redirect/flush 边界补记 driver 已确认 fire 的 item，不重复发射。 |

## 6. 支持边界

- scheduler 主体只完整支持 scalar load/prefetch/store。
- MOU/AMO/CBO/vector 的正向 completion 不属于本轮 split-issue 适配，不能作为合法 scalar
  激励进入 driver。
- `uop_count` 仍是多 uop 预留元信息，本轮没有实现 atomic 多 uop 展开。
- redirect/replay queue 恢复、STD real writeback、pass/fail/terminal 由既有 owner 处理，本轮没有
  修改其算法。
