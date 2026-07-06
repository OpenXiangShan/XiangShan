# memblock agent 添加规则

## 触发条件

当需要重新添加或新增任意 `memblock` UVM agent，或只添加、刷新、替换该 agent 的任一组件时，必须使用本规则。组件包括但不限于：

- `monitor`
- `driver`
- `sequencer`
- `interface`
- `transaction` / `xaction`
- `cfg`
- `default_sequence`
- agent 顶层类与 package/filelist

## 通用中文注释规则

新增或修改 agent xaction、driver、monitor、interface、cfg 中影响测试框架或参考模型行为的核心字段时，必须同时遵循：

```text
mem_ut/ver/ut/memblock/rule/memblock_code_comment_rule.md
```

核心字段含义、合法取值、驱动/采集/状态更新影响必须在源码中添加中文注释。

## 目录归属判断

新增或重新添加 agent 前，必须先判断该接口属于哪一类：

- `memblock` 顶层端口：创建到 `mem_ut/ver/ut/memblock/agent`
- `memblock` 内部模块接口：创建到 `mem_ut/ver/ut/memblock/subagent`

判断依据优先级：

1. 如果接口直接来自 `MemBlock` 顶层端口、`dut_inst.sv` 中 DUT 实例端口，或已有顶层 connect 宏连接到 DUT 顶层端口，按顶层端口处理，放入 `agent`。
2. 如果接口来自 `MemBlock` 内部子模块、内部层级路径、内部 force/bind/探针路径，按内部模块接口处理，放入 `subagent`。
3. 如果接口来源不清楚，先追到 RTL 层级或 connect 关系，确认归属后再创建文件，不要只按信号名猜测。

## 完整 agent 要求

无论用户只要求添加 `monitor`、`driver`、`sequencer`、`interface`、`transaction` 中的一个组件，最终都必须按完整 agent 结构补齐顶层 agent 文件。

完整 agent 至少应包含：

- `<agent_name>.f`
- `<agent_name>_pkg.sv`
- `src/<agent_name>.sv`
- `src/<agent_name>_cfg.sv`
- `src/<agent_name>_interface.sv`
- `src/<agent_name>_xaction.sv`
- `src/<agent_name>_driver.sv`
- `src/<agent_name>_monitor.sv`
- `src/<agent_name>_sequencer.sv`
- `src/<agent_name>_default_sequence.sv`
- 如本地模板要求，还需包含 `src/<agent_name>_dec.sv`

内部模块接口即使只需要某个组件，也必须在 `subagent/<agent_name>` 下创建完整 agent 目录和顶层 agent 类，不能只散落添加单个组件文件。

## tb.f 同步规则

无论 agent 位于 `agent` 还是 `subagent`，都必须同步更新：

```text
mem_ut/ver/ut/memblock/cfg/tb.f
```

要求：

- 顶层端口 agent 添加：
  ```text
  -F ../agent/<agent_name>/<agent_name>.f
  ```
- 内部模块 subagent 添加：
  ```text
  -F ../subagent/<agent_name>/<agent_name>.f
  ```
- 路径应放在现有 agent/subagent filelist 区域，位置保持可读、可维护。
- 只添加组件文件但未把完整 agent filelist 加入 `cfg/tb.f`，视为未完成。

## memblock_env.sv 同步规则

新增或重新添加 agent 后，必须同步完成：

```text
mem_ut/ver/ut/memblock/env/src/memblock_env.sv
```

至少包括：

- 在 `memblock_env` class 中声明 agent 实例。
- 如该 agent 的 monitor 输出需要进入 reference model，声明对应 `uvm_tlm_analysis_fifo`。
- 在 `build_phase` 中创建 agent 实例。
- 在 `build_phase` 中通过 `uvm_config_db` 设置该 agent 的 cfg。
- 在 `connect_phase` 中连接 monitor analysis port 到 fifo/reference model，或明确说明该 agent 不需要接入 RM/scoreboard。

如果新增 agent 需要独立 cfg 字段，也要同步检查并更新：

- `mem_ut/ver/ut/memblock/env/src/memblock_env_cfg.sv`
- `mem_ut/ver/ut/memblock/env/src/memblock_rm.sv`

## 完成标准

完成新增或重新添加 agent 后，至少执行以下静态检查：

- agent 目录完整，package/filelist 能覆盖所有组件。
- `cfg/tb.f` 已包含对应 `-F` 路径。
- `memblock_env.sv` 已完成声明、例化和必要连接。
- interface 信号、transaction 字段、driver/monitor 引用保持一致。

如涉及可编译验证，应按项目远端编译仿真规则从 `mem_ut/ver/ut/memblock/sim` 使用 `eda01` flow。
