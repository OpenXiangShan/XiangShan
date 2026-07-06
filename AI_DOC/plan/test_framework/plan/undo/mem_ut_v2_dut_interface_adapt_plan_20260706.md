# mem_ut V2 DUT Interface 适配 Plan

## Plan 定位

本文规划将当前 `mem_ut_uvm_v2` 分支中的 mem_ut UVM testbench、agent interface、
transaction、driver、monitor 和必要环境连接适配到已生成的 V2 memblock RTL。

本 plan 的权威接口来源是当前 worktree 生成后的 V2 Verilog：

```text
build_memblock/rtl/filelist.f
build_memblock/rtl/MemBlock.sv
build_memblock/rtl/MemBlockTop.sv
```

Scala 源码只用于理解字段语义、valid/ready 时序、bundle 关系和合法行为，不作为最终
端口名、方向、位宽或层级路径依据。

本 plan 是 DUT interface 适配 plan，不实现新的测试激励策略、不修改 V2 设计 RTL、
不重新设计 mem_ut 主测试框架 flow。后续 coding 必须以“让当前 mem_ut 环境正确接入
V2 RTL，并暴露后续功能适配点”为目标。

## 当前基线

当前分支和版本 profile：

```text
branch: mem_ut_uvm_v2
upstream: origin/mem_ut_uvm_v2
V2 design base: origin/kunminghu-v2
```

已读规则和 profile：

```text
mem_ut/ver/ut/memblock/rule/memblock_latest_dut_adapt_rule.md
mem_ut/ver/ut/memblock/rule/memblock_l2tlb_agent_rule.md
mem_ut/ver/ut/memblock/rule/memblock_agent_add_rule.md
mem_ut/ver/ut/memblock/rule/memblock_cfg_add_rule.md
mem_ut/ver/ut/memblock/rule/memblock_parameter_management_rule.md
mem_ut/ver/ut/memblock/rule/version/v2/dut_interface_baseline.md
mem_ut/ver/ut/memblock/rule/version/v2/l2tlb_interface_profile.md
mem_ut/ver/ut/memblock/rule/version/v2/memblock_rtl_profile.md
```

已有 V2 RTL 生成记录：

```text
AI_DOC/analysis/rtl/v2/memblock_rtl_generation_result_20260706.md
```

已有 V2 interface delta 种子文档：

```text
AI_DOC/analysis/interface/v2/memblock_v2_dut_interface_delta_seed_20260706.md
```

当前 V2 RTL 已生成并具备：

```text
build_memblock/rtl/filelist.f
build_memblock/rtl/MemBlock.sv
build_memblock/rtl/MemBlockTop.sv
```

已观察到的 V2 差异种子包括：

| 差异类别 | V2 RTL 事实 | 当前 mem_ut/V3 形态 | 处理原则 |
| --- | --- | --- | --- |
| CPU halt/bypass | `MemBlockTop.sv` 暴露 `outer_cpu_halt`，`MemBlock.sv` 暴露 `io_outer_cpu_halt` | `dut_inst.sv` 和 `backendToTopBypass_agent` 仍使用 `io_ooo_to_mem_backendToTopBypass_cpuWfi` | 先完整确认 V2 的 input/output 方向，再决定是重命名 agent 字段、删除旧输入，还是只 monitor V2 output |
| BPU ctrl 字段命名 | V2 `MemBlock.sv` 端口使用 `io_ooo_to_mem_csrCtrl_bp_ctrl_ubtb_enable`、`btb_enable`、`tage_enable`、`sc_enable` | 当前 `csr_ctrl_agent` 使用 `ubtbEnable`、`abtbEnable`、`mbtbEnable`、`tageEnable`、`scEnable`、`ittageEnable` | 用 V2 RTL 端口全集重建字段映射，删除或合并 V2 不存在字段 |
| LSQ enqueue 通道数 | V2 `MemBlock.sv` 只有 `needAlloc_0..5` 和 `req_0..5_valid` | 当前 `dut_inst.sv`、`lsqenq_agent` 保留 `0..7` 八路字段 | 将通道数收敛到 V2 RTL 真实通道数，并同步 interface/xaction/driver/monitor/sequence |
| L2TLB/PMP 顶层端口 | `MemBlockTop.sv` 暴露 `l2_tlb_req_*` 和 `l2_pmp_resp_*` | 当前 `dut_inst.sv` 中部分 `io_l2_tlb_req_*` 仍接常量，L2TLB agent 连接语义需复核 | 按 V2 L2TLB profile 确认 DTLB/L2TLB 上游 request/response 方向，不能接成 L2Cache/PTW 下游模型 |

以上只是种子，不允许只修这些点后结束。coding 前必须生成完整 V2 RTL 与当前
testbench 连接的端口差异清单。

## 目标

1. 生成完整 V2 DUT 端口和 mem_ut testbench 接线差异清单。
2. 以 V2 Verilog 为权威，修正 `dut_inst.sv` 中 DUT 端口声明和实例连接。
3. 同步受影响的 `*_agent_connect.sv`、agent interface、xaction、driver、monitor。
4. 按 V2 通道数、字段名、方向和位宽修正 sequence、env、RM 或 cfg 中的相关引用。
5. 按 V2 profile 处理 L2TLB/PMP 顶层端口和 responder 语义，避免误接下游模型。
6. 完成静态检查和远端 `tc_sanity/base_fun` 编译验证，形成中文 implementation review。

## 非目标

本 plan 不做以下工作：

1. 不修改生成后的 V2 RTL 来迎合旧 V3 testbench 端口名。
2. 不修改 V2 设计源码以恢复 V3 端口、通道数或字段名。
3. 不新增与 DUT interface 无关的测试激励功能。
4. 不实现新的 coverage、scoreboard 或 RM 算法。
5. 不将 L2TLB agent 改成 L2Cache/PTW/memory 下游模型。
6. 不要求本轮完成 `tc_sanity` runtime pass；本轮最低目标是远端编译通过。若编译通过后运行失败，应记录为后续 runtime/行为适配风险。

## 验收标准

- AC-1：端口差异清单完整
  - 正向检查：生成 V2 RTL `MemBlock.sv` / `MemBlockTop.sv` 端口清单。
  - 正向检查：生成当前 `dut_inst.sv` DUT 实例连接清单。
  - 正向检查：差异清单覆盖新增、删除、重命名、位宽变化、方向变化、通道数变化。
  - 反向检查：不得只依赖首个 VCS 报错或种子文档局部修复。

- AC-2：`dut_inst.sv` 与 V2 RTL 一致
  - 正向检查：DUT 实例端口均能在 V2 RTL 中找到。
  - 正向检查：声明方向和位宽与 V2 RTL 一致。
  - 反向检查：`dut_inst.sv` 不再引用 V2 RTL 不存在的旧 V3 端口，例如 `cpuWfi`、`abtbEnable`、`mbtbEnable`、`ittageEnable`、`enqLsq_*_6/7` 等，除非 review 逐项证明该名称仅保留在历史注释或后续检查命令中。

- AC-3：agent 组件字段一致
  - 正向检查：受影响 agent 的 interface、xaction、driver、monitor 字段集合与 connect 文件一致。
  - 正向检查：driver 只驱动 DUT input；monitor 只采集 DUT output 或必要事务上下文字段。
  - 反向检查：删除或重命名字段不在 constraint、pack/unpack、psdisplay、compare、reset/idle、send_pkt、monitor sample 中残留。

- AC-4：LSQ enqueue 通道数适配 V2
  - 正向检查：`lsqenq_agent`、相关 sequence 和状态构造逻辑使用 V2 的 6 路 `enqLsq` 通道。
  - 正向检查：所有 channel 循环、数组维度、valid/needAlloc 字段、debug dump 和约束统一。
  - 反向检查：不再驱动或采集 V2 RTL 不存在的 `enqLsq` 6/7 通道。

- AC-5：BPU ctrl 字段适配 V2
  - 正向检查：`csr_ctrl_agent` 使用 V2 RTL 真实 snake_case 字段。
  - 正向检查：V2 不存在的旧字段被删除、合并或在 review 中记录为不再适用。
  - 反向检查：不能用 force、宏或 alias 把 V2 RTL 改回 V3 camelCase 接口。

- AC-6：L2TLB/PMP 接入语义正确
  - 正向检查：`l2_tlb_req_*` request/response 和 `l2_pmp_resp_*` 的方向、位宽、ready/valid 时序按 V2 RTL 记录。
  - 正向检查：L2TLB agent 仍表示 DTLB/L2TLB 上游 responder，不变成下游 L2Cache/PTW 模型。
  - 反向检查：若某些端口本轮仍接常量或暂不 takeover，必须在 review 中说明原因、风险和后续落点。

- AC-7：环境集成一致
  - 正向检查：如受影响 agent 类型、字段或 analysis port 改变，`memblock_env.sv`、`memblock_env_cfg.sv`、`memblock_rm.sv`、`cfg/tb.f` 同步。
  - 正向检查：如新增编译期连接开关或默认值，`memblock_compile_params.svh` 和规则文档同步。
  - 反向检查：不新增未受控的 runtime plusarg 来改变 connect-time 静态结构。

- AC-8：验证通过
  - 正向检查：`git diff --check -- mem_ut/ver/ut/memblock AI_DOC` 通过。
  - 正向检查：旧端口/字段残留检查有记录。
  - 正向检查：远端编译 `make eda_compile tc=tc_sanity mode=base_fun` 通过，或 review 中记录唯一剩余 blocker 和对应后续 plan。

## 路径边界

### 最大范围

允许修改：

```text
mem_ut/ver/ut/memblock/tb/**
mem_ut/ver/ut/memblock/agent/**
mem_ut/ver/ut/memblock/subagent/**              # 仅在确认为内部模块接口时使用；当前目录可能尚不存在
mem_ut/ver/ut/memblock/env/**
mem_ut/ver/ut/memblock/seq/**
mem_ut/ver/ut/memblock/cfg/**
mem_ut/ver/ut/memblock/common/**
mem_ut/ver/ut/memblock/rule/**
AI_DOC/analysis/interface/v2/**
AI_DOC/plan/test_framework/plan/undo/**
AI_DOC/plan/test_framework/review_doc/undo/**
```

### 最小范围

最低必须修改或生成：

```text
AI_DOC/analysis/interface/v2/<本轮完整接口差异分析>.md
mem_ut/ver/ut/memblock/tb/dut_inst.sv
所有受影响的 mem_ut/ver/ut/memblock/tb/*_agent_connect.sv
所有受影响 agent 的 interface/xaction/driver/monitor
AI_DOC/plan/test_framework/review_doc/undo/<本 plan 对应 implementation review>.md
```

### 禁止范围

禁止修改：

```text
build_memblock/rtl/**
src/main/scala/**
scripts/generate_memblock_rtl.sh
build.sc
```

如果发现必须重新生成 RTL，应先提交或记录当前接口分析，再按 V2 RTL 生成规则重新生成，
不能直接手改 `build_memblock/rtl`。

## 总体执行 Flow

```text
确认工作区和 V2 RTL 产物
  -> 生成 V2 RTL 端口清单
  -> 生成当前 dut_inst/connect/agent 字段清单
  -> 产出完整 interface delta analysis
  -> 按差异分类拆分修改批次
  -> 修正 dut_inst.sv 顶层端口声明和实例连接
  -> 修正对应 agent connect/interface/xaction/driver/monitor
  -> 修正 env/RM/seq/cfg 中的字段引用和通道数
  -> 执行静态旧字段残留检查
  -> 执行远端 eda_compile
  -> 根据编译结果补齐遗漏接口
  -> 生成 implementation review
```

## 里程碑 1：执行前基线与完整端口差异分析

### 目标

先生成可 review 的端口差异权威输入，避免基于局部报错猜测修改。

### 步骤

1. 检查工作区：

   ```bash
   git status --short --branch
   test "$(git branch --show-current)" = "mem_ut_uvm_v2"
   test -s build_memblock/rtl/filelist.f
   test -s build_memblock/rtl/MemBlock.sv
   test -s build_memblock/rtl/MemBlockTop.sv
   ```

2. 从 V2 RTL 抽取 `MemBlock` 和 `MemBlockTop` 端口清单，至少记录：
   - 端口名。
   - 方向。
   - 位宽。
   - 所属模块。
   - 是否属于顶层 wrapper 暴露端口或内部 `MemBlock` 端口。

3. 从 `dut_inst.sv` 抽取：
   - DUT 实例使用的所有 `.port(signal)`。
   - 对应本地 signal 声明类型和位宽。
   - 接常量的端口。

4. 从 `memblock_connect.sv` 和 `*_agent_connect.sv` 抽取：
   - agent 名。
   - interface 实例名。
   - env agent 路径。
   - `RTL_PATH` 访问字段。
   - `U_IF_NAME` interface 字段。

5. 生成并提交到 analysis 的完整差异文档，建议路径：

   ```text
   AI_DOC/analysis/interface/v2/mem_ut_v2_dut_interface_delta_full_20260706.md
   ```

### 差异文档必须包含

```text
1. V2 RTL 端口清单摘要
2. dut_inst.sv 当前端口清单摘要
3. 新增端口
4. 删除端口
5. 重命名候选
6. 位宽变化
7. 方向变化
8. channel 数量变化
9. L2TLB/PMP 专项端口表
10. 每个变化对应的 mem_ut 文件影响面
```

## 里程碑 2：`dut_inst.sv` 顶层接线适配

### 目标

让 `dut_inst.sv` 的声明和 DUT 实例连接与 V2 RTL 完全一致。

### 重点修改类别

1. CPU halt/bypass：
   - 确认 V2 `outer_cpu_halt` / `io_outer_cpu_halt` 是 DUT output。
   - 确认旧 `io_ooo_to_mem_backendToTopBypass_cpuWfi` 是否已被 V2 删除或替换为其他输入。
   - 如果 V2 仅输出 `outer_cpu_halt`，旧 `cpuWfi` 不应继续作为 DUT input 驱动。
   - `backendToTopBypass_agent` 如仍需要表达 halt 状态，应改为采集或记录 V2 output，而不是驱动旧 input。

2. CSR BPU ctrl：
   - 将旧 `ubtbEnable`、`tageEnable`、`scEnable` 等字段映射到 V2 `*_enable` 字段。
   - 对 V2 不存在的 `abtbEnable`、`mbtbEnable`、`ittageEnable` 等字段，按差异文档结论删除或合并。
   - 不允许用临时 wire alias 掩盖旧字段残留。

3. LSQ enqueue：
   - 将 `needAlloc` 和 `req_valid` 通道数量从当前 8 路收敛到 V2 RTL 真实 6 路。
   - 清理 `dut_inst.sv` 中 `enqLsq_needAlloc_6/7`、`enqLsq_req_6/7_*` 的声明和实例连接。

4. L2TLB/PMP：
   - 按 `MemBlockTop.sv` 顶层端口补齐 `l2_tlb_req_*`、`l2_pmp_resp_*` 的声明和连接。
   - 若某些输入暂时接常量，必须在代码注释和 review 中说明为什么当前阶段允许。
   - 对 output 不允许接常量，应声明 wire 并接入 monitor/agent 或明确记录暂未消费风险。

### 完成检查

```bash
rg -n "cpuWfi|ubtbEnable|abtbEnable|mbtbEnable|tageEnable|scEnable|ittageEnable|enqLsq_(needAlloc|req)_[67]" mem_ut/ver/ut/memblock/tb/dut_inst.sv
```

预期：无有效源码引用；若有命中，必须全部解释。

## 里程碑 3：agent connect 与组件字段同步

### 目标

保证每个受影响 agent 的 connect、interface、xaction、driver、monitor 字段集合一致。

### 受影响 agent 初始清单

| agent | 初始触发原因 | 必查文件 |
| --- | --- | --- |
| `backendToTopBypass_agent_agent` | `cpuWfi` / `outer_cpu_halt` 方向和命名变化 | `tb/backendToTopBypass_agent_connect.sv`、`agent/backendToTopBypass_agent_agent/src/*` |
| `csr_ctrl_agent_agent` | BPU ctrl 字段 snake_case 和字段集合变化 | `tb/csr_ctrl_agent_connect.sv`、`agent/csr_ctrl_agent_agent/src/*` |
| `lsqenq_agent_agent` | `enqLsq` 通道数从 8 路变为 6 路 | `tb/lsqenq_agent_connect.sv`、`agent/lsqenq_agent_agent/src/*` |
| `L2TLB_agent` / `L2tlb_agent` | V2 顶层暴露 `l2_tlb_req_*` 和 `l2_pmp_resp_*` | 现有 L2TLB agent/connect 文件，若不存在则按 agent 添加规则补齐 |

### 修改规则

1. 每个变化字段必须同步：

   ```text
   *_agent_connect.sv
   *_interface.sv
   *_xaction.sv
   *_driver.sv
   *_monitor.sv
   *_default_sequence.sv       # 如字段由 default sequence 驱动
   *_cfg.sv                    # 如字段受 agent cfg 控制
   ```

2. 删除字段时必须检查：
   - `uvm_field_*`。
   - constraint。
   - `psdisplay()`。
   - `compare()`。
   - `copy()` / `clone()` 如存在。
   - driver `reset_phase`、`drive_idle()`、`send_pkt()`。
   - monitor sample 和 analysis port item。

3. 通道数变化时，优先用统一参数或 typedef 表达 V2 通道数。如果当前项目没有此类参数，
   本轮可以先直接收敛字段集合，但 review 必须记录后续是否需要参数化。

4. 新增 L2TLB/PMP agent 或重建 L2TLB agent 时，必须按完整 agent 结构执行，不能只添加
   单个 interface 或 connect 文件。

## 里程碑 4：sequence、env、RM、cfg 影响同步

### 目标

消除 agent 字段变化对运行期 sequence、env 集成和 RM 的影响。

### 必查范围

```text
mem_ut/ver/ut/memblock/env/src/memblock_env.sv
mem_ut/ver/ut/memblock/env/src/memblock_env_cfg.sv
mem_ut/ver/ut/memblock/env/src/memblock_rm.sv
mem_ut/ver/ut/memblock/common/memblock_common/src/*.sv
mem_ut/ver/ut/memblock/seq/**
mem_ut/ver/ut/memblock/cfg/tb.f
mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh
mem_ut/ver/ut/memblock/cfg/user_cfg.sv
mem_ut/ver/ut/memblock/cfg/user_cfg.local.default.sv
```

### 重点同步项

1. LSQ enqueue 6 路化：
   - 生成 transaction 时不得再构造 6/7 通道。
   - driver、sequence、状态表中的循环边界与 V2 通道数一致。
   - debug dump 和统计输出不再打印 V2 不存在通道。

2. CSR BPU ctrl 字段：
   - 若 sequence 或 cfg preset 设置旧字段，必须删除或映射到 V2 字段。
   - 保证默认值不改变 V2 DUT 合法输入语义。

3. CPU halt：
   - 如果从 driver 输入变为 monitor 输出，env/RM 中消费方向也必须同步。
   - 若该字段当前不参与 RM，只需保留 monitor 采集或明确不采集。

4. L2TLB/PMP：
   - 如果启用 L2TLB responder takeover，必须确认 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN` 的默认行为与 V2 顶层端口一致。
   - runtime `MEMBLOCK_L2TLB_SEQ_EN` 不能替代编译期 connect takeover。

## 里程碑 5：静态检查与远端编译闭环

### 静态检查

至少执行：

```bash
git diff --check -- mem_ut/ver/ut/memblock AI_DOC

rg -n "cpuWfi|ubtbEnable|abtbEnable|mbtbEnable|tageEnable|scEnable|ittageEnable|enqLsq_(needAlloc|req)_[67]" \
  mem_ut/ver/ut/memblock

rg -n "l2_tlb_req_|l2_pmp_resp_|L2TLB|L2tlb|l2tlb|DTLB|dtlb" \
  mem_ut/ver/ut/memblock AI_DOC mem_ut/ver/ut/memblock/rule
```

旧字段残留允许范围：

1. 历史 analysis/review 文档中解释旧接口。
2. 本 plan 或 implementation review 中记录旧字段。
3. 明确标注为“V3 旧名”的注释或检查命令。

源码有效逻辑中不得残留旧字段。

### 远端编译

从主仿真目录执行：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

如果仍失败：

1. 先判断是否仍是 DUT 端口/层级/interface mismatch。
2. 如果是，回到里程碑 1 更新差异清单，并继续修复。
3. 如果不是 DUT interface mismatch，记录新 blocker，判断是否属于后续 runtime/行为适配。

### 可选运行

编译通过后可尝试：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_run tc=tc_sanity mode=base_fun
```

若 runtime 失败但不再是 interface/编译问题，本 plan 可以记录为后续 V2 行为适配风险。

## 文档同步

coding 完成后必须新增中文 implementation review，建议路径：

```text
AI_DOC/plan/test_framework/review_doc/undo/mem_ut_v2_dut_interface_adapt_implementation_review_20260706.md
```

review 必须包含：

1. 关联 plan 路径。
2. V2 RTL 端口清单生成方式。
3. 完整接口差异摘要。
4. 每类差异的修改前逻辑、修改后逻辑和源码位置。
5. `dut_inst.sv` 修改说明。
6. 每个受影响 agent 的 connect/interface/xaction/driver/monitor 修改说明。
7. LSQ enqueue 通道数变化说明。
8. BPU ctrl 字段变化说明。
9. CPU halt/bypass 方向变化说明。
10. L2TLB/PMP 语义检查结果。
11. env/RM/seq/cfg 影响检查结果。
12. 执行过的静态检查和远端编译结果。
13. 未完成项和后续 plan 落点。

如果本轮生成新的 interface analysis 文档，应同时在 review 中链接：

```text
AI_DOC/analysis/interface/v2/mem_ut_v2_dut_interface_delta_full_20260706.md
```

## 风险与处理

| 风险 | 影响 | 处理 |
| --- | --- | --- |
| 只按首个 VCS 报错修 | 后续编译反复暴露新端口 mismatch | 先生成完整端口差异清单，再分批修改 |
| 用 V3 旧名 alias V2 端口 | 掩盖真实接口变化，后续 agent 语义错误 | 以 V2 RTL 名称为准，同步 agent 字段 |
| LSQ 通道数只改 dut_inst | driver/sequence 仍生成 8 路，运行期状态错乱 | 同步 interface/xaction/driver/monitor/sequence |
| L2TLB 接成下游模型 | responder 语义错误，TLB lookup 错误 | 严格按 V2 L2TLB profile 判断 DTLB/L2TLB 方向 |
| output 被常量或 driver 驱动 | 编译或仿真语义错误 | output 必须 wire/monitor，input 才由 driver 或常量驱动 |
| 新增 cfg/宏未同步文档 | 后续使用者误解默认行为 | 按 cfg/parameter 规则同步 user_cfg、默认值和规则文档 |

## 后续衔接

本 plan 完成并通过远端编译后，后续可单独开展：

1. V2 runtime 行为适配 plan。
2. V2 L2TLB responder 精细化适配 plan。
3. V2 RM/scoreboard 对齐 plan。
4. V2 directed testcase 与 plus cfg preset 调整 plan。

这些后续任务不能混入本 plan 的 DUT interface 适配 commit。
