# dispatch batch redirect 选择器防护实现 review

## 术语与抽象功能说明

| 英文术语 | 中文含义 | 代码对象 | 使用场景 |
|---|---|---|---|
| `batch` | monitor 在一次服务调用中收集的一组反馈事件 | `process_monitor_event_batch()` | 同时处理普通 writeback、fault 和 redirect |
| `redirect event` | 携带有效 flush/ROB redirect payload 的反馈事件 | `wb_event.redirect.valid` | 选择最老 redirect 并进入恢复流程 |
| `selected` | 当前 batch 中已选出的最老 redirect 事件 | `selected_redirect_event` | 后续候选 redirect 与它比较年龄 |
| `older` | 按 ROB 环回顺序更早、应优先处理 | `redirect_event_is_older()` | 多个 redirect 同批到达时仲裁 |
| `guard` | 在解包 payload 前检查事件类型和有效位 | `event_is_redirect()` | 防止普通/空事件进入 redirect helper |

## Review 范围与问题

本次修改覆盖：

- `mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_batch_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/exception_redirect_replay_handler.sv`

首 seed `710006` 在 RM compare 全部通过后，于 `369813.1ns` 触发
`redirect_from_event requires redirect event`。根因是 redirect 选择器使用
`!found || redirect_event_is_older(...)`，并把尚未初始化为 redirect 的 `selected` 传入比较 helper。

## 修改前逻辑

### 抽象功能描述：`select_oldest_redirect()`

该函数从 batch 事件中筛选 redirect，并返回 ROB 顺序最老的一项；它不负责驱动 redirect 或清理资源。

修改前首次候选和后续候选比较共用一个逻辑表达式。比较函数无条件解包 candidate 和 best 的 redirect
payload，导致空的 `selected` 也可能触发 fatal。

## 修改后逻辑

### 抽象功能描述：`event_is_redirect()`

该函数只判断事件是否携带明确的 redirect payload，并检查冗余 valid 字段的一致性；不改变事件内容。

修改为严格判断：`redirect.valid === 1'b1`。X/Z 或 0 均不会被当作 redirect。

### 抽象功能描述：`redirect_event_is_older()`

该函数只比较两个已经确认是 redirect 的事件，不负责从普通事件构造 payload。

新增入口 guard：任一事件不是严格 redirect 时直接返回不更新，避免调用 `redirect_from_event()`。

### 抽象功能描述：`select_oldest_redirect()`

该函数先保存第一个合法 redirect，之后才比较后续合法 redirect 的 ROB 年龄。

中文伪代码：

1. 遍历 batch；非严格 redirect 事件跳过。
2. 如果当前还没有候选，直接保存该事件并置 `found=1`。
3. 如果已有候选，调用年龄比较 helper；只有新事件更老时才替换候选。
4. 返回是否找到 redirect；普通收尾 batch 返回 0，不进入 redirect payload 解包。

同样逻辑同步应用于 `exception_redirect_replay_handler`，防止异常恢复路径重复出现相同问题。

## 正确性检查

- 普通 issue/writeback/fault 事件：不会调用 `redirect_from_event()`。
- 空 batch 或正常结束 batch：返回未找到 redirect，不伪造 flush。
- 单个 redirect：直接成为候选，不比较空事件。
- 多个 redirect：只在两个有效 redirect 之间按原 ROB 顺序比较。
- X/Z valid：严格过滤，不会误入 redirect 路径。
- RTL 和 DUT 激励：未修改。

## 验证结果

编译目录：
`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/dispatch_batch_guard_compile_20260922`

首 seed 运行目录：
`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/dispatch_batch_guard_run_20260922`

截至 `2026-09-22` 约 `2.58ms` 仿真时间：

- `UVM_FATAL=0`
- `UVM_ERROR=0`
- 原 `redirect_from_event requires redirect event` 未再出现
- 仿真仍在自然运行，尚未达到 100000 请求最终 `$finish`

## 与方案一致性

本次实现与既定最小修复方案一致：显式拆分首次选择和后续比较，并增加严格 redirect 有效性检查；未修改
RTL、Scala、DUT 接口或测试激励。随机回归需在首 seed 自然结束后继续。

