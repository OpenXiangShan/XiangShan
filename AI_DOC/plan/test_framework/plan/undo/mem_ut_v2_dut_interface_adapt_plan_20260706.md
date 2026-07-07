# mem_ut V2 DUT Interface 适配 Plan

## Plan 定位

本文规划将 `mem_ut_uvm_v2` 分支中的 mem_ut UVM testbench、DUT 连接、agent interface、transaction、driver、monitor 和必要编译期连接参数适配到当前生成的 V2 memblock RTL。

本 plan 的权威接口来源是当前 worktree 生成后的 V2 Verilog：

```text
build_memblock/rtl/filelist.f
build_memblock/rtl/MemBlock.sv
build_memblock/rtl/MemBlockTop.sv
```

Scala 源码只用于理解字段语义、valid/ready 时序、bundle 关系和合法行为，不作为最终端口名、方向、位宽或层级路径依据。

本 plan 只做 DUT/interface/connect/agent 字段层面的适配计划，不修改测试框架运行期逻辑。若接口差异会导致 sequence、handler、scheduler、adapter、driver/responder、monitor service loop、env/RM/cfg、状态表生命周期或调度逻辑改变，本 plan 只记录影响、证据和涉及路径，并转入后续测试框架逻辑适配分析 plan：

```text
AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_test_framework_logic_adapt_analysis_plan_20260707.md
```

## 当前基线

当前分支和版本 profile：

```text
branch: mem_ut_uvm_v2
upstream: origin/mem_ut_uvm_v2
V2 design base: origin/kunminghu-v2
```

已有 V2 RTL 生成记录：

```text
AI_DOC/analysis/rtl/v2/memblock_rtl_generation_result_20260706.md
```

已有 V2 interface delta 种子文档：

```text
AI_DOC/analysis/interface/v2/memblock_v2_dut_interface_delta_seed_20260706.md
```

已观察到的差异种子包括 CPU halt/bypass、BPU ctrl 字段命名、LSQ enqueue 通道数、L2TLB/PMP 顶层端口。以上只是种子，不允许只修这些点后结束。

执行本 plan 前必须重新扫描当前 testbench 接口和当前生成的 V2 Verilog 接口，不得只依赖旧 seed 文档或单个 VCS 报错。最终接口结论必须来自当前 `build_memblock/rtl` 和当前 `top_tb.sv` 展开的接线链路。

## 适配原则

1. 版本隔离原则：
   - V2/V3 接口差异优先通过验证环境中的 V2/V3 编译期宏板块隔离适配。
   - 不允许把 V3 旧接口、旧字段名或旧 channel 数强塞到 V2 RTL。
   - 不允许为适配 V2 直接删除仍需保留的 V3 支持。
   - 建议宏名先按 `MEMBLOCK_DUT_V2`、`MEMBLOCK_DUT_V3` 记录；最终 coding 前必须结合
     `mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh` 统一确认实际宏名。
   - V2/V3 版本宏必须互斥；若同时打开或同时关闭，后续 coding plan 必须要求编译期报错。

2. 位宽参数化原则：
   - 位宽、channel 数、idx 宽度、数组维度和 connect-time 静态选择优先做成统一参数或编译期参数宏。
   - V2/V3 使用同一套宏体系选择不同参数值，避免同一语义散落多份硬编码。
   - runtime plusarg 不得改变编译期连接结构。
   - 建议参数名先按 `MEMBLOCK_LSQ_ENQ_NUM`、`MEMBLOCK_ROB_IDX_W`、`MEMBLOCK_LQ_IDX_W`、
     `MEMBLOCK_SQ_IDX_W`、`MEMBLOCK_FU_TYPE_W` 等记录；最终 coding 前必须确认命名和落点。
   - 参数只解决静态结构差异；若字段方向、valid/ready 时序、request/response 生命周期或状态表推进语义不同，必须转入测试框架逻辑适配分析 plan。

3. 运行期逻辑边界原则：
   - 本 plan 不修改 sequence 主循环、handler、scheduler、adapter、monitor service loop、RM 算法或状态表生命周期。
   - 若接口变化影响运行期语义或状态生命周期，只登记影响并转入后续逻辑适配分析 plan。

## 目标

1. 重新生成完整 V2 DUT 端口、当前 testbench 接线和 agent 字段差异清单。
2. 以当前 V2 Verilog 为权威，规划 `dut_inst.sv` 顶层端口声明和实例连接适配。
3. 同步规划受影响的 `*_agent_connect.sv`、agent interface、xaction、driver、monitor 字段集合。
4. 对 V2/V3 字段、位宽、channel 数和 idx 宽度差异给出编译期宏或参数化隔离方案。
5. 按 V2 profile 记录 L2TLB/PMP 顶层端口和 responder 语义，避免误接下游模型。
6. 对可能影响 sequence、env、RM、cfg、状态表或调度逻辑的点只做影响登记，并转入后续逻辑适配分析 plan。
7. 给出静态检查、远端编译和 implementation review 要求。

## 非目标

1. 不修改生成后的 V2 RTL 来迎合旧 V3 testbench 端口名。
2. 不修改 V2 设计源码以恢复 V3 端口、通道数或字段名。
3. 不直接删除 V3 支持；涉及公共文件时必须保留可维护的 V2/V3 隔离路径。
4. 不新增与 DUT interface/connect/agent 字段无关的测试激励功能。
5. 不修改 sequence、handler、scheduler、adapter、monitor service loop、RM 算法或状态表生命周期。
6. 不实现新的 coverage、scoreboard 或 RM 算法。
7. 不将 L2TLB agent 改成 L2Cache/PTW/memory 下游模型。
8. 不要求本轮完成 `tc_sanity` runtime pass；本轮最低目标是远端编译通过。

## 验收标准

- AC-1：接口扫描重新完成
  - 正向检查：重新生成 V2 RTL `MemBlock.sv` / `MemBlockTop.sv` 端口清单。
  - 正向检查：重新生成当前 `dut_inst.sv`、`top_tb.sv`、`memblock_connect.sv`、所有 `*_agent_connect.sv` 和受影响 agent 字段清单。
  - 正向检查：差异清单覆盖新增、删除、重命名、位宽变化、方向变化、channel 数变化和 bundle 层级变化。
  - 反向检查：不得只依赖旧 seed 文档、历史 V3 认知或首个 VCS 报错局部修复。

- AC-2：`dut_inst.sv` 与 V2 RTL 一致
  - 正向检查：DUT 实例端口均能在当前 V2 RTL 中找到。
  - 正向检查：声明方向和位宽与当前 V2 RTL 一致。
  - 反向检查：`dut_inst.sv` 不再有效引用 V2 RTL 不存在的旧 V3 端口。

- AC-3：版本隔离可审查
  - 正向检查：V2/V3 接口差异通过编译期宏、版本宏板块或统一参数隔离。
  - 正向检查：公共文件中 V2 和 V3 路径的条件编译边界清楚，默认值有文档说明。
  - 反向检查：不能用临时 alias、force 或旧字段包装把 V3 接口强塞到 V2 RTL；不能为 V2 适配直接删除 V3 支持。

- AC-4：位宽和 channel 数参数化
  - 正向检查：LSQ enqueue channel 数、idx 宽度、数组维度和循环边界优先来自统一参数或编译期宏。
  - 正向检查：V2/V3 使用相同宏体系选择不同参数值。
  - 反向检查：源码有效逻辑中不得散落多个互相独立的硬编码 channel 数或位宽常量。

- AC-5：agent 组件字段一致
  - 正向检查：受影响 agent 的 interface、xaction、driver、monitor 字段集合与 connect 文件一致。
  - 正向检查：driver 只驱动 DUT input；monitor 只采集 DUT output 或必要事务上下文字段。
  - 反向检查：删除或重命名字段不在 constraint、pack/unpack、psdisplay、compare、reset/idle、send_pkt、monitor sample 中残留。

- AC-6：L2TLB/PMP 接入语义正确
  - 正向检查：`l2_tlb_req_*` request/response 和 `l2_pmp_resp_*` 的方向、位宽、ready/valid 时序按当前 V2 RTL 记录。
  - 正向检查：L2TLB agent 仍表示 DTLB/L2TLB 上游 responder，不变成下游 L2Cache/PTW 模型。

- AC-7：运行期逻辑影响只登记不实现
  - 正向检查：sequence、env、RM、cfg、状态表、scheduler 或 adapter 的潜在影响有清单。
  - 正向检查：凡需要改变运行期语义或状态生命周期的项，均转入 `mem_ut_v2_test_framework_logic_adapt_analysis_plan_20260707.md`。
  - 反向检查：本 plan 的 coding 不引入新的测试框架运行期逻辑策略。

- AC-8：验证闭环可执行
  - 正向检查：`git diff --check -- mem_ut/ver/ut/memblock AI_DOC` 通过。
  - 正向检查：旧端口/字段残留检查有记录。
  - 正向检查：远端编译 `make eda_compile tc=tc_sanity mode=base_fun` 通过，或 review 中记录唯一剩余 blocker 和对应后续 plan。

## 路径边界

后续执行本 plan 时允许修改：

```text
mem_ut/ver/ut/memblock/tb/**
mem_ut/ver/ut/memblock/agent/**
mem_ut/ver/ut/memblock/subagent/**
mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh
mem_ut/ver/ut/memblock/cfg/tb.f
AI_DOC/analysis/interface/v2/**
AI_DOC/plan/test_framework/plan/undo/**
AI_DOC/plan/test_framework/review_doc/undo/**
```

若仅为字段声明、类型、analysis port 连接或 agent 例化一致性所需，可检查并最小修改：

```text
mem_ut/ver/ut/memblock/env/src/memblock_env.sv
mem_ut/ver/ut/memblock/env/src/memblock_env_cfg.sv
mem_ut/ver/ut/memblock/common/memblock_common/src/*.sv
```

以下范围本 plan 只允许扫描、记录影响和转入后续逻辑适配分析 plan，不在本 plan 中实现运行期逻辑变化：

```text
mem_ut/ver/ut/memblock/seq/**
mem_ut/ver/ut/memblock/env/src/memblock_rm.sv
handler / scheduler / adapter / monitor service loop 相关文件
状态表生命周期相关文件
```

禁止修改：

```text
build_memblock/rtl/**
src/main/scala/**
scripts/generate_memblock_rtl.sh
build.sc
```

## 里程碑

1. 执行前基线与完整端口差异分析：
   - 重新扫描当前 V2 RTL、`dut_inst.sv`、`top_tb.sv`、`memblock_connect.sv`、`*_agent_connect.sv` 和受影响 agent 字段。
   - 产出完整 interface delta analysis，包含纯 connect 差异、可参数化字段差异、运行期逻辑影响三类结论。
   - 每个差异必须记录以下字段：

     ```text
     差异项
     V2 RTL 事实
     当前 mem_ut/V3 假设
     影响文件
     是否可由 V2/V3 宏隔离
     是否可由静态参数隔离
     是否需要测试框架逻辑适配分析
     ```

2. 版本宏和参数隔离方案：
   - 盘点现有版本宏、编译期参数和 include 入口。
   - 对 channel 数、idx 宽度、数组维度、字段集合差异建立统一参数名。
   - 将 connect-time 静态选择放入 `memblock_compile_params.svh` 或既有版本宏体系。

3. `dut_inst.sv` 顶层接线适配：
   - 按当前 V2 RTL 修正端口声明、方向、位宽和实例连接。
   - CPU halt/bypass、BPU ctrl、LSQ enqueue、L2TLB/PMP 必须逐项检查。

4. agent connect 与组件字段同步：
   - 同步对应 `*_agent_connect.sv`、interface、xaction、driver、monitor。
   - 如果字段变化要求运行期语义变化，只登记影响，不在本 plan 中实现。

5. 运行期逻辑影响登记：
   - 对 sequence、env、RM、cfg、状态表、scheduler、adapter、monitor service loop 做影响清单。
   - 将需要逻辑分析的项转入 `mem_ut_v2_test_framework_logic_adapt_analysis_plan_20260707.md`。

6. 静态检查与远端编译闭环：
   - 执行旧字段残留检查、`git diff --check` 和远端 `eda_compile`。
   - 生成中文 implementation review。

## 风险与处理

| 风险 | 影响 | 处理 |
| --- | --- | --- |
| 只按旧 seed 或首个 VCS 报错修 | 后续编译反复暴露新端口 mismatch | 执行前重新扫描当前 V2 RTL、当前 testbench 接线和当前 agent 字段 |
| 用 V3 旧名 alias V2 端口 | 掩盖真实接口变化，后续 agent 语义错误 | 以 V2 RTL 名称为准，同步 agent 字段；V2/V3 差异用编译期宏隔离 |
| 为 V2 适配直接删除 V3 支持 | 公共验证环境失去 V3 可维护性 | 公共文件保留 V2/V3 条件编译块，版本默认值写入 review |
| 位宽和 idx 宽度散落硬编码 | 后续 V2/V3 差异难以维护 | 建立统一参数或编译期宏，保持单一权威来源 |
| LSQ 通道数只改 `dut_inst.sv` | driver/agent 字段仍不一致，编译或运行期状态错乱 | 同步 connect/interface/xaction/driver/monitor，并用统一参数表达 channel 数 |
| L2TLB 接成下游模型 | responder 语义错误，TLB lookup 错误 | 严格按 V2 L2TLB profile 判断 DTLB/L2TLB 方向 |
| 接口差异实际影响运行期逻辑 | 本 plan 范围膨胀，混入 sequence/RM/状态表修改 | 本 plan 只登记影响，转入逻辑适配分析 plan |
| 新增 cfg/宏未同步文档 | 后续使用者误解默认行为 | 按 cfg/parameter 规则同步默认值、作用对象和 review 说明 |

## 后续衔接

本 plan 完成并通过远端编译后，后续可单独开展：

1. V2 测试框架运行期逻辑适配分析 plan。
2. V2 L2TLB responder 精细化适配 plan。
3. V2 RM/scoreboard 对齐 plan。
4. V2 directed testcase 与 plus cfg preset 调整 plan。

这些后续任务不能混入本 plan 的 DUT/interface/connect/agent 字段适配 commit。
