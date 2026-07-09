# mem_ut interface 变更总结文档规则

本文约束 `mem_ut` / `memblock` 验证环境发生 DUT/interface/agent 接口变更后，必须输出的接口变更总结文档格式、检查范围、判定方法和 review 要求。

新增或修改本文档所约束的总结文档时，还必须遵循：

- `AI_DOC/project_management/ai_doc_file_management_rule.md`
- `AI_DOC/project_management/ai_doc_language_rule.md`
- `mem_ut/ver/ut/memblock/rule/memblock_latest_dut_adapt_rule.md`
- `mem_ut/ver/ut/memblock/rule/memblock_agent_add_rule.md`

## 1. 触发条件

出现以下任一情况时，必须生成或刷新 interface 变更总结文档：

- DUT 顶层端口发生新增、删除、重命名、位宽变化或方向变化。
- `dut_inst.sv`、`memblock_connect.sv` 或任意 `tb/*_agent_connect.sv` 发生接口连接调整。
- 任意 agent 的 `interface`、`xaction` / `transaction`、`monitor`、`driver` 字段发生新增、删除、重命名或位宽变化。
- 从 V2/V3 或其他版本切换 RTL 基准后，需要确认当前 agent 划分是否仍完整覆盖接口。
- 用户要求检查接口是否有遗漏、是否和 DUT 对齐、是否有多余字段、是否需要新增 agent。

## 2. 文档落点和命名

接口变更总结文档属于分析文档，默认放在：

```text
AI_DOC/analysis/interface/<v2|v3>
```

文件名必须包含版本、主题和日期。推荐格式：

```text
mem_ut_<v2|v3>_agent_interface_signal_matrix_YYYYMMDD.md
mem_ut_<v2|v3>_interface_change_summary_YYYYMMDD.md
```

文档正文必须使用中文。信号名、类名、路径、宏名、命令和日志可以保留英文或代码原文。

## 3. 权威输入

生成总结文档前必须明确并写入文档：

- 当前分支。
- 当前版本 profile，例如 V2 或 V3。
- RTL 权威来源，例如 `build/rtl/MemBlock.sv` 或当前版本 profile 指定的 `MemBlock.sv`。
- DUT 实例入口：`mem_ut/ver/ut/memblock/tb/dut_inst.sv`。
- connect 入口：`mem_ut/ver/ut/memblock/tb/memblock_connect.sv` 和所有活动的 `tb/*_agent_connect.sv`。
- agent interface 来源：`mem_ut/ver/ut/memblock/agent/*/src/*_interface.sv`，如存在 subagent 也必须纳入对应目录。
- transaction 来源：对应 `*_xaction.sv` 或 `*_transaction.sv`。
- monitor 来源：对应 `*_monitor.sv`。
- driver 来源：对应 `*_driver.sv`。

如果当前版本 profile 与用户指定版本冲突，必须先停止并确认，不得按猜测生成文档。

## 4. 必须检查的覆盖项

文档必须按照当前 agent 划分逐个列出 interface 信号。每个 agent 至少包含：

- agent 名称。
- interface 文件路径。
- transaction / xaction 文件路径。
- connect 文件路径。
- monitor 文件路径；如果该 agent 没有 monitor，必须写明“无 monitor”。
- driver 文件路径；如果该 agent 没有 driver，必须写明“无 driver”。

每个 interface 信号必须逐项检查并写入表格：

| 字段 | 要求 |
|---|---|
| 信号 | interface 中的完整信号名。 |
| 位宽 | interface 声明位宽；1-bit 可以写 `1` 或保持空位宽但必须口径一致。 |
| interface 方向 | 如存在 clocking block，写明 `drv_cb` / `mon_cb` 方向。 |
| transaction 字段 | 有对应字段写“有”，没有写“没有”。 |
| connect | 有连接写“有”，没有写“没有”。 |
| connect 方向/对象 | 尽量解析为 `IF->RTL`、`RTL->IF`、`常量->IF`、`IF->本地`、`本地->IF`；无法解析时写“有连接，未解析对象”。 |
| monitor 采集 | monitor 中有实际采集写“有”，没有写“没有”；无 monitor 时写“无 monitor”。 |
| driver 驱动 | driver 中有实际驱动写“有”，没有写“没有”；无 driver 时写“无 driver”。 |

表格中不能只写总体结论，必须保留逐信号明细。

## 5. 判定规则

### 5.1 transaction 字段判定

同名字段存在于对应 `*_xaction.sv` 或 `*_transaction.sv` 中，记为“有”。

如果字段经过结构化封装或数组化改名，必须在说明中写明映射关系，不能静默记为“有”。

如果 interface 字段不应进入 transaction，例如纯本地时钟、reset、少数 handshake 辅助字段，必须在总结中单独解释，不能只留空。

### 5.2 connect 判定

connect 判定以活动分支为准，优先检查 `MEMBLOCK_UT` 或当前编译宏实际启用的分支。

以下情况可记为“有”：

- `force RTL_PATH.<signal> = U_IF_NAME.<signal>`，记为 `IF->RTL`。
- `force U_IF_NAME.<signal> = RTL_PATH.<signal>`，记为 `RTL->IF`。
- `force U_IF_NAME.<signal> = '0` 或其他常量，记为 `常量->IF`。
- 连接到 testbench 本地 wire/reg，记为 `IF->本地` 或 `本地->IF`。

如果 interface 中有字段但 connect 中没有任何连接，必须记为“没有”，并在缺失项汇总中列出。

### 5.3 monitor 采集判定

monitor 中实际读取 `mon_cb.<signal>`、`vif.<signal>` 或等价采样路径，并写入 transaction、analysis port、raw queue 或内部检查状态，记为“有”。

仅出现在注释、声明、未使用局部变量或 TODO 中，不得记为“有”。

如果该 agent 是纯 driver agent，没有 monitor，需要在 agent 概述和信号表中写明“无 monitor”，并说明是否符合设计意图。

### 5.4 driver 驱动判定

driver 中实际对 `drv_cb.<signal>`、`vif.<signal>` 或等价路径赋值，记为“有”。

仅出现在注释、声明、打印、约束或 monitor 采样中，不得记为“有”。

“没有 driver 驱动”是静态覆盖结果，不自动等同于缺陷。对于 DUT output、被动 monitor 型信号、由 RTL 侧产生的 ready/valid 握手信号，没有 driver 可以是合理状态。对于 DUT input 且测试目标需要主动建模的字段，必须在总结中标为后续风险或待适配项。

## 6. 文档必须包含的总结

文档末尾必须包含以下总结章节。

### 6.1 缺失项汇总

至少按以下类别汇总：

- 缺 transaction 字段的 interface 信号。
- 缺 connect 的 interface 信号。
- 缺 monitor 采集的 interface 信号。
- 缺 driver 驱动的 interface 信号。
- 无 monitor / 无 driver 的 agent。

每类缺失项必须列出：

- agent。
- 信号名。
- 当前状态。
- 是否属于合理例外。
- 后续处理建议。

### 6.2 DUT 顶层端口未归属 agent 汇总

必须从当前 RTL `MemBlock` 顶层端口和 `dut_inst.sv` 实例连接出发，列出没有归属到现有 agent 的 DUT 顶层端口。

每个未归属端口至少写明：

- DUT 端口名。
- 方向。
- 位宽。
- `dut_inst.sv` 中连接的 testbench 信号。
- 是否已有默认常量、wire/reg 或本地连接。
- 建议新建或扩展的 agent 分类。

分类建议必须按信号语义给出，不能只按端口名前缀机械归类。典型分类包括：

- TileLink / bus 边界 agent。
- perf event output monitor agent。
- perf event input stimulus 或常量源 agent。
- trace / encoder bypass agent。
- L2 TLB requestor agent。
- L2 PMP response monitor agent。
- L2 prefetch control monitor agent。
- interrupt / debug interrupt sink agent。
- WFI / power state monitor agent。
- reset / frontend control agent。
- other_ctrl 扩展。
- 需要 RTL 语义二次确认的未分类项。

对于 DUT input 方向端口，不得默认建议纯 monitor；必须说明是否需要 driver、常量源或外部模型。

### 6.3 版本差异和风险

如果本次是 V2/V3 或其他版本切换导致的接口变化，必须单独说明：

- 哪些字段是旧版本有、新版本没有。
- 哪些字段是新版本有、旧版本没有。
- 哪些字段只是命名变化但语义可能一致。
- 哪些字段位宽变化。
- 哪些字段影响测试框架 raw queue、RM、scoreboard、sequence 或公共状态。

不允许把测试框架语义问题伪装成 connect 层常量补丁；涉及测试框架的事项必须记录到后续测试框架 plan 或 TODO。

## 7. Review 要求

接口变更总结文档生成后必须进行独立复查。复查至少确认：

- agent 数量和 interface 信号数量与源码一致。
- 每个 interface 信号没有遗漏、重复或错误归类。
- transaction、connect、monitor、driver 的“有/没有”判定能回溯到源码。
- DUT 未归属顶层端口数量和分类合计自洽。
- DUT input/output 方向没有导致错误的新建 agent 建议。
- 版本 profile 和 RTL 权威来源写明且没有混用。
- 文档正文为中文。

如果任务采用 subagent flow，推荐由 subagent 生成初版文档，主 agent 进行 review；或者主 agent 生成初版文档，subagent 做独立 review。最终必须以最后一轮 review 无必须修复项作为完成标准。

## 8. 建议检查命令

执行时可按实际版本和路径调整命令：

```bash
find mem_ut/ver/ut/memblock/agent -path '*_interface.sv' | sort
find mem_ut/ver/ut/memblock/subagent -path '*_interface.sv' | sort
rg -n "U_IF_NAME\\.|RTL_PATH\\.|force " mem_ut/ver/ut/memblock/tb/*_agent_connect.sv
rg -n "mon_cb\\.|drv_cb\\." mem_ut/ver/ut/memblock/agent mem_ut/ver/ut/memblock/subagent
rg -n "module MemBlock|input |output " build/rtl/MemBlock.sv build_memblock/rtl/MemBlock.sv
git diff --check -- AI_DOC AGENTS.md mem_ut/ver/ut/memblock/rule
```

如果某个路径不存在，不应直接忽略；必须根据当前版本 profile 和实际 RTL 生成规则确认正确路径。

## 9. 完成标准

接口变更总结文档完成时至少满足：

- 已按 agent 逐信号列出 interface 覆盖矩阵。
- 每个信号的 transaction、connect、monitor、driver 状态均明确写“有”或“没有”。
- 缺失项已集中汇总，并区分合理例外和待修复风险。
- `dut_inst.sv` / DUT 顶层未归属 agent 的端口已完整列出并给出分类建议。
- 分类建议符合 DUT 端口方向和信号语义。
- 已通过独立 review，最后一轮 review 无必须修复项。
- 已执行必要的格式检查。
