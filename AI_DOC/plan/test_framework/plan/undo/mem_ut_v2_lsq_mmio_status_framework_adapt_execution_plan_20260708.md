# mem_ut V2 LSQ MMIO/status 测试框架适配执行 Plan

## 1. Plan 定位

本文是 V2 `lsqio.loadMmio/loadMmioUop/storeMmio/storeMmioUop/pendingMMIOld/pendingst/scommit` 状态信号的测试框架适配执行 plan。目标是明确这些 V2 状态是否参与当前公共状态推进。

本轮默认 coding 选择：不扩展 raw ctrl 字段，不把 MMIO/status debug 字段接入 terminal/pass/fail，只在 monitor/adapter 附近补充注释、局部分类表或 review 结论。若执行时证明某 MMIO/status 字段是当前主 flow 必需，必须停止本 plan 并新建 raw ctrl 扩展专项 coding plan。

不改变 LSQ commit/deq 主体 flow，不把 debug/status 信号混入 pass/fail。

## 2. 范围边界

涉及文件：

```text
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/agent/lsqcommit_agent_agent/src/lsqcommit_agent_agent_driver.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv
AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_lsq_mmio_status_framework_adapt_execution_plan_20260708.md
```

不属于本 plan：

- 不实现 MMIO 正确性 checker。
- 不新增 PMP/L2 TLB 顶层 output monitor。
- 不改变 terminal_done 语义，除非明确证明某状态是当前 flow 终态推进必需。
- 不在本 plan 中新增 `memblock_sync_pkg` raw ctrl 字段；raw ctrl 扩展必须另建 plan。

### 2.1 执行前 RTL 基线确认

执行本 plan 前必须从仓库根目录确认当前 V2 RTL 权威输入真实存在：

```bash
test -e build/rtl/MemBlock.sv
test -e build_memblock/rtl/MemBlock.sv
test -e build_memblock/rtl/filelist.f
```

若任一文件不存在，必须先确认当前 worktree 的 RTL 生成状态和 V2 profile，不得继续沿用不存在的 `build_memblock/rtl/MemBlockTop.sv` 或同级旧 worktree 作为接口事实来源。本 plan 默认只分类 MMIO/status 字段，也需要以真实 RTL 端口为准判断字段是否存在和是否影响主 flow；该检查不代表本 plan 会直接修改 RTL。

## 3. 问题依据

V2-only LSQ 状态包括：

```text
io_mem_to_ooo_lsqio_loadMmio_0/1/2
io_mem_to_ooo_lsqio_loadMmioUop_0/1/2_robIdx_value
io_mem_to_ooo_lsqio_storeMmio
io_mem_to_ooo_lsqio_storeMmioUop_robIdx_value
io_ooo_to_mem_lsqio_pendingMMIOld
io_ooo_to_mem_lsqio_pendingst
io_ooo_to_mem_lsqio_scommit
```

当前 `io_mem_to_ooo_ctrl_agent_agent_monitor.sv` 已采样 load/store MMIO 字段，但 raw ctrl 入队条件只包括：

```text
lqDeq
sqDeq
memoryViolation
dispatch_flushsb_waiting_empty
```

当前 `lsqcommit_agent_agent_driver.sv` 能驱动 `pendingMMIOld/pendingst/scommit`，但 idle 只默认 0，缺少与 LSQ commit/deq flow 的语义说明。

## 4. 修改原因

V2 比 V3 提供更细粒度的 MMIO 状态。如果这些字段影响当前 commit、pending 或终态推进，就不能只采样后丢弃；如果不影响当前 smoke，也不能把它们误用于 pass/fail。

需要先建立分类：

- `FLOW_REQUIRED`：当前状态推进必需，必须进入 raw event/handler。
- `DEBUG_ONLY`：只用于日志或后续 coverage/RM，不影响状态推进。
- `UNUSED_IN_SMOKE`：当前 testcase 不覆盖，只做 X/Z 或暂不接入。

## 5. 修改后方案

### 5.1 output 状态分类

默认建议：

| 信号 | 分类 | 原因 |
|---|---|---|
| `lqDeq/sqDeq/lqDeqPtr/sbIsEmpty` | `FLOW_REQUIRED` | 已参与 LSQ deq/flushSb 状态推进 |
| `memoryViolation` | `FLOW_REQUIRED` | 已转换 redirect event |
| `loadMmio/storeMmio` | 初始 `DEBUG_ONLY` | 当前 terminal_done 不依赖 MMIO 标志 |
| `loadMmioUop/storeMmioUop` | 初始 `DEBUG_ONLY` | 可记录 ROB value，但不直接 pass/fail |
| `pendingMMIOld/pendingst/scommit` | driver control | 默认 0；后续 directed commit/MMIO plan 再驱动 |

如果执行时发现当前 testcase 必须用 MMIO 状态推进，不能在本 plan 中直接扩大 raw ctrl；必须先新建 raw ctrl 扩展专项 coding plan，并在新 plan 中补齐字段定义、写者、读者、默认值和 terminal 影响结论。

### 5.2 raw ctrl 扩展策略

本轮默认不扩展 raw ctrl。允许的 coding 仅限：

- 在 `io_mem_to_ooo_ctrl_agent_agent_monitor.sv` 已采样 MMIO 字段的位置补充注释，说明这些字段当前为 `DEBUG_ONLY`。
- 在 `dispatch_monitor_event_adapter.sv` 处理 ctrl raw event 的位置补充注释，说明 raw ctrl 入队条件仍只由 deq、memory violation、flushSb empty 等主 flow 字段决定。
- 在本 plan 或后续 implementation review 中维护分类表，列出 MMIO/status 字段当前不影响 terminal/pass/fail。

若未来专项 plan 决定扩展 raw ctrl，必须至少定义以下内容，且该内容不属于本 plan 默认 coding 范围：

```text
bit [2:0] load_mmio_valid;
bit [2:0][MEMBLOCK_ROB_VALUE_W-1:0] load_mmio_rob_value;
bit store_mmio_valid;
bit [MEMBLOCK_ROB_VALUE_W-1:0] store_mmio_rob_value;
```

raw ctrl 扩展专项还必须说明字段 reset/default、monitor 写者、adapter/handler 读者、ROB value 无 flag 时的处理，以及为什么不会改变 terminal_done。由于 V2 MMIO uop 只有 ROB value，无 flag 时不得直接构造完整 ROB key；需要通过 active window 或 active ROB map 受限反查，无法反查则只记录 debug，不更新终态。

### 5.3 driver control 策略

`lsqcommit_agent_agent_driver` 默认继续驱动：

```text
pendingMMIOld = 0
pendingst = 0
scommit = 0
```

若后续 testcase 需要 directed 驱动，必须新增 sequence 参数和 plan，不能在本 plan 中随机驱动这些状态。

## 6. 函数/任务级伪代码

### 6.1 可选局部分类表或 `classify_v2_lsq_status_fields()` 注释

目的：在 monitor、adapter 注释或本 plan/review 分类表中统一记录 V2 LSQ status 分类，防止采样字段被误用。

输入：当前采样字段和当前 testcase 配置。

输出/副作用：只形成分类结论；默认不新增函数、不新增 raw ctrl 字段、不直接更新公共状态。

源码级伪代码：

```text
// 可选局部 helper；也可以只实现为注释和 review 表，不要求新增函数。
function bit classify_v2_lsq_status_fields(sample, output class_result);
    class_result.lq_deq_required = sample.lq_deq != 0;
    class_result.sq_deq_required = sample.sq_deq != 0;
    class_result.mem_violation_required = sample.memory_violation_valid;
    class_result.mmio_debug_only = sample.load_mmio != 0 || sample.store_mmio;
    return class_result.lq_deq_required ||
           class_result.sq_deq_required ||
           class_result.mem_violation_required ||
           dispatch_flushsb_waiting_empty;
endfunction
```

中文文字伪代码：

该分类把 V2 LSQ 状态分成两组。第一组是已经影响状态推进的 deq、memory violation 和 flushSb empty 等字段；只要这些字段有效，就继续按现有 raw ctrl 条件生成 event。第二组是 MMIO debug 字段；当前默认只在注释或 review 表中记录为 debug，不作为 raw ctrl 入队条件，也不推进 pass/fail。这样 monitor 采样到 MMIO 不会导致公共状态误更新。

### 6.2 可选后续专项 `fill_raw_ctrl_mmio_debug()`

函数目的：如果后续专项 plan 决定记录 MMIO debug 信息，把采样值放入 raw ctrl 的 debug 字段或日志，不改变 terminal_done。

输入：V2 MMIO sample、raw ctrl。

输出/副作用：

- 可选填 raw debug 字段。
- 可选打印 `uvm_info`。
- 不调用 pass/fault/terminal 更新函数。

源码级伪代码：

```text
function void fill_raw_ctrl_mmio_debug(sample, ref raw_ctrl);
    raw_ctrl.load_mmio_valid = sample.load_mmio;
    raw_ctrl.load_mmio_rob_value = sample.load_mmio_rob_value;
    raw_ctrl.store_mmio_valid = sample.store_mmio;
    raw_ctrl.store_mmio_rob_value = sample.store_mmio_rob_value;
endfunction
```

中文文字伪代码：

该函数只搬运 MMIO 观察字段。它把 load/store MMIO 标志和对应 ROB value 写入 raw ctrl 的 debug 字段，供日志、后续 review 或未来 RM plan 使用。它不解析 uid，不修改 status，也不触发 terminal_done。本轮不扩展 raw ctrl，因此该函数不是本 plan 必须新增函数；执行者最多增加注释或 review 表，说明后续若扩展 raw ctrl 才需要该 helper。

### 6.3 `apply_raw_ctrl_deq()`

函数目的：保持 deq/commit 主路径不变，只在必要时忽略 MMIO debug 字段。

源码级伪代码：

```text
function void apply_raw_ctrl_deq(raw);
    data.update_sb_is_empty(raw.sb_is_empty);
    if (raw has mmio debug) log only;
    if (raw.lq_deq == 0 && raw.sq_deq == 0) return;
    monitor_commit_handler.apply_raw_ctrl_deq(raw.lq_deq, lq_ptr, raw.sq_deq, sq_ptr);
endfunction
```

中文文字伪代码：

该逻辑仍先更新 store buffer empty 状态。MMIO debug 字段当前不进入 raw ctrl，因此不会出现在该函数的状态推进输入里。只有 LQ/SQ deq 数量非 0 时，才调用 LSQ commit handler 推进 deq 状态。这样 V2 MMIO 观察不会改变原有 deq flow，也不会把 debug 状态误当成 transaction 完成条件。

## 7. 验收标准

1. V2 MMIO/status 字段有明确分类：`FLOW_REQUIRED`、`DEBUG_ONLY` 或 `UNUSED_IN_SMOKE`。
2. `loadMmio/storeMmio` 默认不改变 pass/fail/terminal_done。
3. `pendingMMIOld/pendingst/scommit` 默认值和驱动时机有说明；本 plan 不随机驱动。
4. 本 plan 执行后没有新增 raw ctrl 字段；若发现必须新增，已停止并新建专项 plan。
5. 不新增每拍全表扫描；ROB value 无 flag 时不能直接构造完整 key。

## 8. 验证命令或静态检查

```bash
git diff --check -- mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent mem_ut/ver/ut/memblock/agent/lsqcommit_agent_agent mem_ut/ver/ut/memblock/common mem_ut/ver/ut/memblock/seq/base_seq_help AI_DOC
rg -n "loadMmio|storeMmio|pendingMMIOld|pendingst|scommit" mem_ut/ver/ut/memblock
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
```

## 9. 与原始/初步 plan 差异说明

初步 plan 只提出 V2 MMIO/LSQ-ROB 状态需要结论。本文给出三类分类和默认处理：deq/violation 保持主路径，MMIO 初始只作为 debug，pending/scommit 默认 0 并留给后续 directed plan。

## 10. 风险与非目标

风险：

- 如果 V2 DUT 因 pending/scommit 默认 0 无法推进某些场景，需要另建 directed LSQ commit/MMIO plan。
- ROB value 无 flag 的 MMIO uop 不能可靠反查完整 ROB key。

非目标：

- 不实现 MMIO checker。
- 不实现 PMP/L2 TLB 顶层 response 观察。
- 不修改 terminal_done 定义。

## 11. 与原测试框架逻辑对比和修改类型总结

修改类型结论：`无代码优先检查/复查 + 仅字段/参数适配`。本 plan 默认只做字段分类和影响复查，不要求运行期主逻辑修改。只有执行时证明某 MMIO/status 字段是当前主 flow 必需，才停止并另建 raw ctrl 扩展专项；本 plan 不直接扩大 raw ctrl 或 terminal/pass/fail。

原测试框架逻辑：

- `io_mem_to_ooo_ctrl_agent_agent_monitor::mon_data()` 每拍采样 ctrl/status 字段，包括 `lqDeq/sqDeq`、memory violation、`sbIsEmpty`、以及 V2 `loadMmio/storeMmio` 观察字段。
- 当前 raw ctrl 入队条件只包含 LQ/SQ deq、memory violation 或 flushSb waiting empty 等主 flow 字段。
- `dispatch_monitor_event_adapter::collect_ctrl_redirect_events_batch()` 消费 raw ctrl，先调用 `apply_raw_ctrl_deq()` 更新 SB empty 和 LSQ deq，再通过 `convert_raw_memory_violation()` 生成 redirect event。
- `lsqcommit_agent_agent_driver` 驱动 pending/scommit/flushSb 输入；当前 idle 默认 0，非本 plan 随机驱动。

本 plan 修改后逻辑：

- 默认只补充分类表或注释，说明 V2 `loadMmio/storeMmio` 和对应 ROB value 是 `DEBUG_ONLY` 或 `UNUSED_IN_SMOKE`。
- `apply_raw_ctrl_deq()` 主路径保持不变，仍只由 deq 数量推进 LSQ commit handler。
- `pendingMMIOld/pendingst/scommit` 仍按现有 sequence/driver 策略驱动，不在本 plan 中新增 directed 随机控制。
- 若发现某 MMIO/status 字段影响当前 testcase 闭环，必须新建专项 plan，定义 raw struct、monitor 写者、adapter/handler 读者、默认值和 terminal 影响。

逻辑改变项：

- 默认无运行期逻辑改变。
- 可选的 `classify_v2_lsq_status_fields()` 只是文档/review 分类，不是 SV 主路径函数。
- 可选后续 `fill_raw_ctrl_mmio_debug()` 明确不在本 plan 落地；若后续专项落地，也只记录 debug，不更新 status 或 terminal。

字段/参数改变项：

- 分类字段包括 `loadMmio_0/1/2`、`loadMmioUop_*_robIdx_value`、`storeMmio`、`storeMmioUop_robIdx_value`。
- 驱动输入字段包括 `pendingMMIOld`、`pendingst`、`scommit`，本 plan 只记录默认/边界，不新增 plus/cfg。
- 不新增 `dispatch_raw_ctrl_t` 字段；不新增 env/plus 参数。

性能/生命周期影响：

- RTL 基线路径确认只发生在执行前准备阶段，用于防止误读不存在的 `MemBlockTop.sv` 或错误 worktree，不属于测试框架 runtime 逻辑改变。
- 不新增每拍扫描、ROB value 反查或 active window 查找。
- 不改变 raw ctrl queue 入队条件、消费顺序和 `apply_raw_ctrl_deq()` 调用点。
- 不改变 LSQ commit pointer、SB empty、terminal_done、pass/fail。
- ROB value 无 flag 的 MMIO 观察不进入 uid/status 解析，避免引入不可靠生命周期。

覆盖性结论：

本 plan 覆盖 V2 LSQ MMIO/status 字段对主 flow 的影响分类，结论是默认不影响当前测试框架主体逻辑。若后续证明 MMIO 是当前主功能必需，应另建专项，不能在本 plan 中混入 raw ctrl 扩展。该 flow 没有遗漏，但当前只要求分类和边界复查。
