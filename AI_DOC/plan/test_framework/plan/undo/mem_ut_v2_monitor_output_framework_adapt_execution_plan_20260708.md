# mem_ut V2 monitor output 与 V2-only output 分类适配执行 Plan

## 1. Plan 定位

本文是 V2 顶层 agent monitor 输出策略和 V2-only output 分类的正式执行 plan。目标是逐 agent 明确 monitor 职责，避免把“未写 analysis port”误判为一定错误，也避免当前主 flow 必需的 raw queue 字段缺失。

本轮范围收敛为“分类审查 + 当前主 flow 必需 RAW_QUEUE monitor 的最小修复”。本文不默认恢复大量 `ANALYSIS_PORT` transaction；analysis port 恢复必须另建 RM/coverage 或 monitor 专项 plan。

## 2. 范围边界

涉及文件：

```text
mem_ut/ver/ut/memblock/agent/*/src/*_monitor.sv
mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv
mem_ut/ver/ut/memblock/tb/dut_inst.sv
mem_ut/ver/ut/memblock/tb/*_agent_connect.sv
AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_monitor_output_framework_adapt_execution_plan_20260708.md
```

不属于本 plan：

- 不修改 int writeback raw event 语义；该内容由 int writeback plan 处理。
- 不新增 V2-only output agent，除非分类证明当前主功能必需。
- 不写 covergroup 或 scoreboard。
- 不恢复 analysis port transaction，除非另有专项 plan 证明 RM/coverage/checker 当前必需。

### 2.1 执行前 RTL 基线确认

执行本 plan 前必须从仓库根目录确认当前 V2 RTL 权威输入真实存在：

```bash
test -e build/rtl/MemBlock.sv
test -e build_memblock/rtl/MemBlock.sv
test -e build_memblock/rtl/filelist.f
```

若任一文件不存在，必须先确认当前 worktree 的 RTL 生成状态和 V2 profile，不得继续沿用不存在的 `build_memblock/rtl/MemBlockTop.sv` 或同级旧 worktree 作为接口事实来源。本 plan 多数步骤只分类 monitor output 和 V2-only output，也必须以真实 RTL 端口为准；该检查不代表本 plan 会直接修改 RTL。

## 3. 问题依据

当前多个顶层 agent monitor 只做 X/Z 检查或 raw queue 写入，`mon_item_port.write(mon_tr)` 大多注释。

V2-only 顶层 output 包括：

```text
io_l2_tlb_req_resp_*
io_l2_pmp_resp_*
io_outer_l2PfCtrl_*
io_wfi_wfiSafe
```

其中 `io_l2_tlb_req_resp_*` 是 V2 顶层 L2/L2Cache 侧 TLB response，不是当前内部 `L2TLB_agent` 接管点。

## 4. 修改原因

monitor 输出路径有两类消费者：

- 测试框架公共状态：通常走 `memblock_sync_pkg` raw queue。
- RM/scoreboard/coverage：通常需要 analysis port transaction。

如果不分类：

- 可能强行恢复所有 monitor transaction，制造无用对象和字段维护成本。
- 也可能漏掉某些当前主功能必需的 raw queue。
- V2-only 顶层 output 可能被误接到错误 agent。

## 5. 修改后方案

### 5.1 monitor 分类

每个 monitor 归入以下一类：

| 分类 | 含义 | coding 行为 |
|---|---|---|
| `XZ_ONLY` | 只检查 X/Z，不服务状态流 | 不恢复 analysis port，review 说明原因 |
| `RAW_QUEUE` | 写 `memblock_sync_pkg` raw queue | 保证 raw 字段完整，不强制 `mon_item_port.write()` |
| `ANALYSIS_PORT` | RM/scoreboard/coverage 当前需要 transaction | 本 plan 只记录分类，不 coding；另建专项 plan |
| `RAW_AND_ANALYSIS` | 两条路径都需要 | 本 plan 只修 raw queue 必需字段；analysis 另建专项 plan |

初始建议：

```text
io_mem_to_ooo_ctrl_agent: RAW_QUEUE
io_mem_to_ooo_int_wb_agent: RAW_QUEUE
io_mem_to_ooo_iq_feedback_agent: RAW_QUEUE
csr_ctrl_agent: RAW_QUEUE 或 RAW_AND_ANALYSIS，取决于 RM 需求
L2tlb_agent: responder/internal, 默认不作为普通 analysis port
其他未被公共状态消费 agent: XZ_ONLY，除非 RM 明确依赖
```

执行本 plan 后必须在本文、implementation review 或同目录 review 段落中生成/更新 monitor 分类表，至少包含：

| agent/monitor | 分类 | 是否本轮 coding | coding 原因或不 coding 原因 |
|---|---|---|---|
| `io_mem_to_ooo_ctrl_agent` | `RAW_QUEUE` | 仅当 raw 主 flow 字段缺失 | LSQ deq/redirect/flushSb 状态推进 |
| `io_mem_to_ooo_int_wb_agent` | `RAW_QUEUE` | 仅当 raw 主 flow 字段缺失 | int writeback 状态推进 |
| `io_mem_to_ooo_iq_feedback_agent` | `RAW_QUEUE` | 仅当 raw 主 flow 字段缺失 | issue feedback 状态推进 |
| `csr_ctrl_agent` | `RAW_QUEUE` | 仅当 runtime CSR raw 字段缺失 | CSR snapshot 更新 |
| `L2tlb_agent` | responder/internal | 不在 monitor output plan 中恢复 analysis | DTLB->L2TLB responder 模型 |
| 其他 monitor | `XZ_ONLY` 或待定 | 默认不 coding | 无当前主 flow 消费者 |

### 5.2 V2-only output 分类

默认处理：

| output | 默认分类 | 原因 |
|---|---|---|
| `io_l2_tlb_req_resp_*` | `UNUSED_IN_CURRENT_MAIN_FLOW` | 顶层 L2 侧 TLB response，不接内部 `L2TLB_agent` |
| `io_l2_pmp_resp_*` | `UNUSED_IN_CURRENT_MAIN_FLOW` | 当前 smoke 不依赖顶层 PMP response 推进状态 |
| `io_outer_l2PfCtrl_*` | `UNUSED_IN_CURRENT_MAIN_FLOW` | prefetch control 后续专项 |
| `io_wfi_wfiSafe` | `STATUS_OBSERVATION_ONLY` | 不作为通过条件 |

reset/halt/WFI 边界说明：

- `io_reset_backend` 和 `io_outer_cpu_halt` 已由 `other_ctrl_agent` 采样并做 X/Z/status 观察，默认不进入 dispatch raw queue，不作为 pass/fail 或 terminal_done 条件。
- `memblock_sync_pkg::reset_backend_done` 是 testbench 在 `top_tb.sv` 中维护的仿真同步标志，用于 gate driver/monitor，不等同于 DUT output `io_reset_backend`。
- V2 `io_outer_cpu_halt` 不应继续写成旧 V3 `cpuWfi/io_outer_cpu_wfi` 同义替换；`io_wfi_wfiSafe` 仍按本表归为 `STATUS_OBSERVATION_ONLY`，未建专项前不得作为测试通过条件。

若某 output 被当前 testcase/RM/checker 明确依赖，另建 agent、RM/coverage 或 monitor 专项 plan，并按 agent 添加规则补齐结构；不得在本 plan 中混入 RM/checker/coverage 实现。

## 6. 函数/任务级伪代码

### 6.1 `classify_monitor_role()`

函数目的：执行 plan 时逐 monitor 得出职责分类。

输入：

- monitor 是否写 raw queue。
- monitor 是否仅 X/Z。
- RM/env 是否连接 analysis export。
- V2-only output 是否当前主功能必需。

输出/副作用：

- 输出分类表。
- 不直接改状态。

源码级伪代码：

```text
for each monitor:
    has_raw_queue_write = rg "push_raw|set_latest_raw" monitor
    has_analysis_write = rg "mon_item_port.write" monitor
    rm_depends = rg "<agent_name>.*analysis|mon_item_port" env rm
    if has_raw_queue_write && rm_depends: RAW_AND_ANALYSIS
    else if has_raw_queue_write: RAW_QUEUE
    else if rm_depends: ANALYSIS_PORT
    else: XZ_ONLY
```

中文文字伪代码：

执行者逐个 monitor 检查是否写入 raw queue、是否已经写 analysis port、env/RM 是否连接该 monitor transaction。如果 monitor 已经服务公共状态 raw queue，但 RM 不依赖标准 transaction，就归为 `RAW_QUEUE`。如果 RM 或 coverage 当前需要 transaction，则归为 `ANALYSIS_PORT` 或 `RAW_AND_ANALYSIS`。如果 monitor 只采样并做 X/Z，就归为 `XZ_ONLY`，不强行恢复 transaction。

### 6.2 后续专项 `emit_analysis_transaction_if_required()`

函数目的：仅在后续 RM/coverage/monitor 专项 plan 中，对 `ANALYSIS_PORT` 或 `RAW_AND_ANALYSIS` monitor 恢复 transaction 输出。本 plan 不要求新增该函数或恢复 `mon_item_port.write()`。

输入：采样字段、monitor 分类。

输出/副作用：必要时创建 `mon_tr`、赋字段、`unpack()`、`mon_item_port.write(mon_tr)`。

源码级伪代码：

```text
if (role inside {ANALYSIS_PORT, RAW_AND_ANALYSIS}) begin
    mon_tr = xaction::type_id::create("mon_tr");
    assign all sampled fields required by xaction;
    mon_tr.channel_id = cfg.channel_id;
    mon_tr.unpack();
    mon_item_port.write(mon_tr);
end
```

中文文字伪代码：

该逻辑只在后续专项证明 analysis 输出必需时启用。它创建 monitor transaction，把本拍采样字段完整赋给 transaction，再设置 channel_id 并调用 `unpack()`。最后通过 `mon_item_port.write()` 发给 analysis 订阅者。当前 plan 只记录哪些 monitor 可能需要专项，不在主 flow 适配中恢复无消费者 transaction。

### 6.3 `classify_v2_only_output_group()`

函数目的：判断 V2-only 顶层 output 是否需要进入 agent。

源码级伪代码：

```text
for each v2_only_output_group:
    if affects current raw queue state progress: FLOW_REQUIRED
    else if rm_or_checker_depends_now: ANALYSIS_REQUIRED
    else if only debug/status observation: STATUS_ONLY
    else: UNUSED_IN_CURRENT_MAIN_FLOW
```

中文文字伪代码：

执行者对每个 V2-only output 先判断它是否影响当前公共状态推进；如果影响，就必须进入 raw queue 或对应 handler。如果不影响状态，但当前 RM/checker 已经依赖，就需要 analysis monitor。如果只是 debug/status，就记录为观察项，不作为 testcase pass/fail。完全不被当前主功能使用的 output 保持 dut_inst 连接即可，不新增 agent。

## 7. 验收标准

1. 每个顶层 agent monitor 都有分类表。
2. `RAW_QUEUE` monitor 的 raw queue 字段完整且有消费者。
3. `ANALYSIS_PORT/RAW_AND_ANALYSIS` monitor 只记录专项需求，不在本 plan 中恢复 `mon_tr` 输出。
4. `XZ_ONLY` monitor 在分类表或 review 中说明不输出 transaction 的原因。
5. V2-only output 四组均有主功能影响结论。
6. `io_l2_tlb_req_resp_*` 不被接入内部 `L2TLB_agent`。
7. 执行产物包含 monitor 分类表，列出每个 agent 分类和是否本轮 coding。

## 8. 验证命令或静态检查

```bash
git diff --check -- mem_ut/ver/ut/memblock/agent mem_ut/ver/ut/memblock/env mem_ut/ver/ut/memblock/common mem_ut/ver/ut/memblock/tb AI_DOC
rg -n "mon_item_port.write|push_raw|set_latest_raw|io_l2_tlb_req_resp|io_l2_pmp_resp|io_outer_l2PfCtrl|io_wfi_wfiSafe" mem_ut/ver/ut/memblock
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

本 plan 默认不恢复 monitor transaction。若后续专项恢复 monitor transaction 且影响运行期状态，再执行：

```bash
make eda_run tc=tc_sanity mode=base_fun
```

## 9. 与原始/初步 plan 差异说明

原始 monitor plan 说明了“analysis port 大多未写”和 V2-only output 待分析。本文将其收敛为可执行的分类审查和 RAW_QUEUE 最小修复流程，明确本轮不恢复大量 analysis port transaction，并要求产出逐 agent 分类表。

## 10. 风险与非目标

风险：

- 若 RM 当前隐式依赖某 monitor transaction，需要执行者通过 env/RM 连接复查发现。
- 恢复大量 analysis port 可能增加仿真开销，因此必须按分类启用。

非目标：

- 不实现 RM/checker/coverage。
- 不新增 V2-only output agent，除非分类证明当前主功能必需。

## 11. 与原测试框架逻辑对比和修改类型总结

修改类型结论：`无代码优先检查/复查 + 仅字段/参数适配`，必要时包含 `局部逻辑适配`。本 plan 默认只做 monitor/V2-only output 字段分类和影响复查；只有发现 `RAW_QUEUE` 主 flow 必需字段缺失时才做局部字段修复。默认不恢复大量 analysis port，不改变 runtime 主逻辑。

原测试框架逻辑：

- 顶层 agent monitor 大多每拍采样 interface 字段并执行 X/Z 检查，但 `mon_item_port.write(mon_tr)` 多数处于注释状态。
- 当前测试框架主 flow 主要依赖 `memblock_sync_pkg` raw queue：int writeback raw、IQ feedback raw、ctrl raw、CSR raw、sfence raw 等。
- `dispatch_monitor_event_adapter` 消费 raw queue 并转换为公共状态事件；analysis port 当前不是 dispatch 主状态推进的必需路径。
- V2-only 顶层 output 已在 `dut_inst`/connect 层存在或待分类，但多数还没有专用 agent 采样策略。

本 plan 修改后逻辑：

- 对每个 monitor 分类为 `XZ_ONLY`、`RAW_QUEUE`、`ANALYSIS_PORT` 或 `RAW_AND_ANALYSIS`。
- `RAW_QUEUE` 类 monitor 只检查 raw queue 字段是否完整且有消费者；如果缺字段，按对应专项 plan 局部修 raw。
- `ANALYSIS_PORT/RAW_AND_ANALYSIS` 默认只记录后续专项需求，不在本 plan 中批量恢复 `mon_item_port.write()`。
- V2-only output 分组判断是否影响当前主状态推进；`io_l2_tlb_req_resp_*` 和 `io_l2_pmp_resp_*` 不接入内部 `L2TLB_agent`。

逻辑改变项：

- 默认无运行期逻辑改变。
- 若发现 raw queue 主 flow 字段缺失，本 plan 只允许最小修复 monitor raw 写入或把问题转入已有专项，例如 int writeback plan、CSR/control plan、LSQ MMIO/status plan。原因是主状态推进依赖 raw queue，而不是 analysis port。
- 批量恢复 analysis port 属于后续专项，不属于本 plan 主逻辑。

字段/参数改变项：

- 分类表覆盖顶层 agent monitor 的输出职责。
- V2-only output 分类覆盖 `io_l2_tlb_req_resp_*`、`io_l2_pmp_resp_*`、`io_outer_l2PfCtrl_*`、`io_wfi_wfiSafe` 等组。
- 不新增 env/plus 参数；不新增 raw struct 字段，除非另一个专项明确要求。
- 20260709 signal matrix 中大量 perf event/input stimulus 类问题归 DUT/interface 或独立 agent 输入刺激适配，不作为本 8 个测试框架主体 flow 的遗漏。

性能/生命周期影响：

- RTL 基线路径确认只发生在执行前准备阶段，用于防止误读不存在的 `MemBlockTop.sv` 或错误 worktree，不属于测试框架 runtime 逻辑改变。
- 默认不创建额外 `mon_tr` 对象，不增加 analysis port 广播开销。
- 不改变 raw queue 消费顺序、active map、prefix/cursor、terminal_done 或 pass/fail。
- 若后续专项恢复 analysis port，必须单独评估对象创建和订阅者影响。

覆盖性结论：

本 plan 覆盖 monitor output/V2-only output 分类 flow。它负责判断哪些 output 影响当前测试框架主状态，哪些只是观察或后续 RM/checker 入口。结论是：默认只是分类和 RAW_QUEUE 完整性复查，不影响测试框架主体逻辑；未纳入的 perf event/input stimulus 等属于 DUT/interface 或新增 agent 范围，不是当前 V2 测试框架 runtime flow 遗漏。
