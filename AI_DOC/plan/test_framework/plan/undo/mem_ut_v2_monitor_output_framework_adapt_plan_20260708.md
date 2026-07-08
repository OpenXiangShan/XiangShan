# mem_ut V2 monitor 输出与新增顶层 output 测试框架适配 Plan

## 1. Plan 定位

本文记录 V2 DUT 接口对齐 review 中发现的测试框架运行期待办项。

本 plan 不处理 `io_mem_to_ooo_int_wb_agent` 写入 `dispatch_raw_int_wb_t` 的字段语义，该部分由以下 plan 单独跟踪：

```text
AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_int_wb_writeback_framework_adapt_plan_20260708.md
```

本 plan 覆盖两类后续测试框架适配：

1. 顶层 agent monitor 是否需要输出标准 UVM transaction。
2. V2 新增顶层 output 是否影响当前主功能、RM/checker 观察点或后续 testcase。

## 2. 问题来源

### 2.1 monitor analysis port 输出策略未闭合

当前多个顶层 agent monitor 已经采样 interface，并且部分 monitor 还执行 X/Z 检查或写入 `memblock_sync_pkg` raw queue，但标准 UVM transaction 输出链路大多仍处于注释状态：

```text
mon_tr = new();
mon_tr.<field> = <sampled_field>;
mon_tr.unpack();
mon_item_port.write(mon_tr);
```

这会导致依赖 monitor analysis port 的 RM、scoreboard 或 coverage 无法直接获得 transaction。当前部分 flow 可能绕过 analysis port，直接消费 raw queue，因此不能简单要求所有 agent 都恢复 `mon_item_port.write(mon_tr)`，需要按 agent 职责逐个分类。

### 2.2 V2 新增顶层 output 尚未完成主功能影响分析

以下 V2 顶层 output 当前只在 `dut_inst.sv` 中实例化，尚未进入 agent：

```text
io_l2_tlb_req_resp_*
io_l2_pmp_resp_*
io_outer_l2PfCtrl_*
io_wfi_wfiSafe
```

这些信号属于 V2 顶层真实 output，但当前没有证据表明它们是 `tc_sanity/base_fun` 主激励闭环的必要观察点。后续需要先按测试目标判断是否影响主功能，再决定是否接入 agent。

## 3. 修改 Flow 伪代码

代码式伪代码：

```text
for each top-level agent monitor:
    classify monitor_role as XZ_ONLY / RAW_QUEUE / ANALYSIS_PORT / RAW_AND_ANALYSIS
    if monitor_role needs analysis transaction:
        restore or rewrite mon_tr field assignment
        call mon_tr.unpack()
        call mon_item_port.write(mon_tr)
    else:
        document why analysis transaction is not required

for each V2-only top-level output group:
    analyze testcase, RM, checker and coverage dependency
    if output affects current main function:
        choose target agent or new monitor
        define transaction fields and sampling rule
    else:
        keep dut_inst-only connection
        document non-connection reason
```

文字伪代码：

后续执行时，先逐个查看顶层 agent monitor 的实际职责。如果该 monitor 只负责 X/Z 检查，就在文档中说明不需要输出 transaction；如果它负责向公共 raw queue 写事件，就继续确认 raw queue 是否已经覆盖测试框架需要的全部信息；如果 RM、scoreboard 或 coverage 需要标准 UVM transaction，则恢复或重写 `mon_tr` 创建、字段赋值、`unpack()` 和 `mon_item_port.write(mon_tr)`。对 V2 新增顶层 output，先根据 testcase、RM、checker 和 coverage 是否依赖这些信号判断主功能影响；只有确认需要观察时才新增 agent 或并入现有控制类 agent，否则保留在 `dut_inst.sv` 中实例化但不进入 agent。

## 4. 待办项

### 4.1 顶层 agent monitor 分类

后续需要为每个顶层 agent 给出以下分类结论：

| 分类 | 含义 | 后续处理 |
|---|---|---|
| `XZ_ONLY` | 只采样并做 X/Z 检查 | 不恢复 `mon_item_port.write()`，但在 review 中说明原因 |
| `RAW_QUEUE` | 采样后写入 `memblock_sync_pkg` raw queue | 检查 raw queue 是否覆盖公共状态需要的字段 |
| `ANALYSIS_PORT` | RM/scoreboard/coverage 需要标准 transaction | 恢复或重写 `mon_tr` 输出逻辑 |
| `RAW_AND_ANALYSIS` | 同时需要 raw queue 和标准 transaction | 两条路径都要同步字段和时序 |

### 4.2 V2 新增顶层 output 主功能影响分析

| 接口 | 初步语义 | 待分析问题 | 默认处理 |
|---|---|---|---|
| `io_l2_tlb_req_resp_*` | V2 顶层 L2 侧 TLB response | 是否被当前 mem_ut 主功能、RM 或 checker 观察；是否仅属于 L2/L2Cache 侧查询响应 | 不误接到内部 `L2TLB_agent`；若不影响当前主功能，暂不接入 agent |
| `io_l2_pmp_resp_*` | V2 顶层 L2 侧 PMP response | 是否影响当前地址权限、异常或 fault 判断闭环 | 若当前主功能不依赖，暂不接入 agent |
| `io_outer_l2PfCtrl_*` | 外部 L2 prefetch control 相关 output | 是否影响当前 prefetch 场景或低功耗/控制观察 | 若当前 testcase 不覆盖，暂不接入 agent |
| `io_wfi_wfiSafe` | WFI safe 状态 output | 是否影响当前 halt/WFI/低功耗测试目标 | 若当前主功能不依赖，暂不接入 agent |

判断规则：

1. 若接口不影响当前主功能激励闭环、公共状态推进、RM/checker 必要观察点和已规划 testcase，则本轮不接入 agent，只在后续专项测试目标中再处理。
2. 若接口影响主功能或后续 testcase/RM/checker 必须观察，则再决定新增独立 monitor/agent，或并入现有 `other_ctrl_agent` 等控制类 agent。
3. `io_l2_tlb_req_resp_*` 不得误接到当前内部 `L2TLB_agent`，因为当前 `L2TLB_agent` 接管的是内部 DTLB 到 L2TLB/PTW responder 通路，不是 V2 顶层 L2 侧 TLB/PMP requestor response。

## 5. 影响文件

后续 coding 前至少需要检查：

```text
mem_ut/ver/ut/memblock/agent/*/src/*_monitor.sv
mem_ut/ver/ut/memblock/agent/*/src/*_xaction.sv
mem_ut/ver/ut/memblock/env/src/memblock_env.sv
mem_ut/ver/ut/memblock/env/src/memblock_rm.sv
mem_ut/ver/ut/memblock/common/memblock_common/src/*.sv
mem_ut/ver/ut/memblock/tb/dut_inst.sv
mem_ut/ver/ut/memblock/tb/*_agent_connect.sv
```

如果确认 V2 新增顶层 output 需要进入 agent，还需要按 `memblock_agent_add_rule.md` 补齐 interface、xaction、monitor、driver 和 `cfg/tb.f`、`memblock_env.sv`。

## 6. 验收标准

1. 每个顶层 agent monitor 都有明确职责分类。
2. 需要输出 transaction 的 monitor 已完成 `mon_tr` 字段赋值、`unpack()` 和 `mon_item_port.write(mon_tr)`。
3. 不需要输出 transaction 的 monitor 在 review 中写清原因，避免误判为遗漏。
4. `io_l2_tlb_req_resp_*`、`io_l2_pmp_resp_*`、`io_outer_l2PfCtrl_*` 和 `io_wfi_wfiSafe` 均有主功能影响结论。
5. 若 V2 新增顶层 output 不影响当前主功能，不新增 agent；若影响，则补齐对应 agent 或 monitor 链路。
6. `io_l2_tlb_req_resp_*` 不被误接到内部 `L2TLB_agent`。
7. 通过 `git diff --check -- mem_ut/ver/ut/memblock AI_DOC`。
8. 若修改 monitor 输出或新增 agent，后续需要执行远端编译；若影响运行期状态流，还需要执行 `tc_sanity/base_fun` 仿真。
