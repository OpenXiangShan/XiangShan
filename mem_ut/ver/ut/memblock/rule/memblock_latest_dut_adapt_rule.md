# memblock 最新 DUT 适配规则

## 触发条件

当出现以下任一情况时，必须使用本规则：

- 用户要求适配最新 DUT、同步最新 RTL 接口、检查 DUT 接口变化
- 更新最新代码并重新生成 `build_memblock/rtl` 后
- VCS 编译出现 DUT 端口、层级路径、interface 信号或 agent 字段不匹配
- 新增、删除、重命名或调整 memblock 顶层端口、内部模块接口、probe/bind/force 路径

本规则用于保证 UVM 环境和最新 Verilog DUT 接口一致。Scala 源码只用于理解逻辑语义，实际接口以生成后的 Verilog RTL 和 testbench 接线为准。

不同 XiangShan 版本的 DUT 顶层端口、内部层级和 agent connect 基线可能不同。
执行适配前必须按当前分支或用户指定版本读取对应 profile：

```text
mem_ut/ver/ut/memblock/rule/version/v2/dut_interface_baseline.md
mem_ut/ver/ut/memblock/rule/version/v3/dut_interface_baseline.md
```

## 固定入口

适配最新 DUT 时，必须从 testbench 顶层接线入口开始检查：

```text
mem_ut/ver/ut/memblock/tb/top_tb.sv
```

`top_tb.sv` 中所有和 RTL 交接的 include 与宏都属于检查范围，包括：

```text
mem_ut/ver/ut/memblock/tb/dut_inst.sv
mem_ut/ver/ut/memblock/tb/tc_if_connect.sv
mem_ut/ver/ut/memblock/tb/memblock_connect.sv
mem_ut/ver/ut/memblock/tb/*_agent_connect.sv
```

`memblock_connect.sv` 展开的每个 `*_agent_connect.sv` 都视为一个 DUT-interface 交接点。不得只根据单个编译报错局部修补，必须按交接点完整比对。

## 权威来源优先级

接口判断优先级如下：

1. 生成后的 Verilog RTL：
   ```text
   build_memblock/rtl/MemBlock.sv
   build_memblock/rtl/MemBlockTop.sv
   build_memblock/rtl/filelist.f
   ```
2. `top_tb.sv` 展开的 testbench 接线链路。
3. `src/main/scala/xiangshan` 中的 Scala 源码，仅用于理解 valid/ready、idx、bundle 语义和合法行为，不作为最终端口名、位宽、方向依据。

## 检查流程

1. 展开 testbench 接线清单：
   - 从 `top_tb.sv` 找到 `dut_inst.sv`、`tc_if_connect.sv`、`memblock_connect.sv`。
   - 从 `memblock_connect.sv` 找到所有 `*_agent_connect.sv`。
   - 对每个 connect 宏记录 agent 名、interface 实例名、env agent 路径、RTL 路径、连接的 RTL 端口或内部层级信号。

2. 对比 Verilog RTL：
   - 顶层端口必须和 `MemBlock` / `MemBlockTop` 的端口名、方向、位宽一致。
   - 内部模块接口必须确认层级路径仍存在，模块名、实例名、generate 名、信号名、位宽都一致。
   - 对数组化 channel、Vec 展开、Decoupled/ValidIO、Valid/Ready、bits 子字段，必须逐字段对比。

3. 分类变化：
   - 新增信号
   - 删除信号
   - 重命名信号
   - 位宽变化
   - 方向变化
   - channel 数量变化
   - bundle 层级展开变化
   - valid/ready 或请求/响应语义变化
   - 内部层级路径变化

4. 按影响面同步修改，不允许只修改一处让编译暂时通过。

## 同步修改规则

### dut_inst.sv

如果 DUT 顶层端口变化，必须同步更新：

```text
mem_ut/ver/ut/memblock/tb/dut_inst.sv
```

要求：

- DUT input 由 testbench/driver 驱动时声明为 `reg` 或本环境既有驱动类型。
- DUT output 由 testbench/monitor 采集时声明为 `wire` 或本环境既有采集类型。
- 位宽必须和 RTL 完全一致。
- 端口实例连接必须无遗漏、无悬空、无旧信号残留。

### *_agent_connect.sv

如果某个交接点变化，必须同步更新对应：

```text
mem_ut/ver/ut/memblock/tb/<agent>_connect.sv
```

要求：

- interface 信号和 RTL 端口/层级信号连接一一对应。
- 新增信号必须接入 interface 或明确记录为什么不进入该 agent。
- 删除或重命名信号必须清理旧连接，不能保留失效层级路径。
- 内部模块路径变化时，必须追到最新 RTL 实例路径后再改，不能只按旧名字猜测。

### interface

对应 agent 的 interface 必须同步更新：

```text
mem_ut/ver/ut/memblock/agent/<agent>/src/<agent>_interface.sv
```

如该 agent 属于内部模块 subagent，则按 `memblock_agent_add_rule.md` 中的目录归属规则定位实际 agent 目录。

要求：

- interface 字段集合、方向用途、位宽和 connect 宏一致。
- valid/ready、请求/响应、数组 channel 的字段必须保持成组更新。
- reset 后需要默认值的字段必须能被 driver idle/reset 逻辑覆盖。

### transaction / xaction

对应 transaction 必须同步更新：

```text
mem_ut/ver/ut/memblock/agent/<agent>/src/<agent>_xaction.sv
```

要求：

- 新增 interface 字段后，transaction 中必须有对应字段，除非该字段只作为纯握手 ready 且明确不需要进入 transaction。
- 删除或重命名 interface 字段后，必须删除或重命名 transaction 字段。
- 位宽、数组维度、枚举/idx 类型必须和 RTL 接口一致。
- `constraint`、`pack()`、`unpack()`、`psdisplay()`、`compare()` 中不能残留旧字段。
- valid/ready、robIdx、lqIdx、sqIdx、uopIdx 等 idx 字段如果位宽或合法范围变化，约束必须同步变化。

### driver

对应 driver 必须同步更新：

```text
mem_ut/ver/ut/memblock/agent/<agent>/src/<agent>_driver.sv
```

要求：

- driver 只能驱动 DUT input 方向信号，不能驱动 DUT output。
- `reset_phase`、`drive_idle()`、`send_pkt()` 中的字段集合必须和 interface/transaction 对齐。
- valid/ready 信号必须遵守最新 RTL 握手机制。
- 新增输入字段必须在 reset/idle 和有效 transaction 驱动路径都有定义。
- 删除字段必须从所有驱动路径清理。

### monitor

对应 monitor 必须同步更新：

```text
mem_ut/ver/ut/memblock/agent/<agent>/src/<agent>_monitor.sv
```

要求：

- monitor 采集字段必须和最新 interface/transaction 对齐。
- DUT output 必须采集；如果 reference model 或 scoreboard 需要完整事务，也可以采集同一事务中的 DUT input。
- valid/ready 采样点必须和最新 RTL 时序一致。
- 只有事务完整时才写 analysis port，不能把半拍或失配字段送入 RM。

## 关联文件检查

如接口变化影响环境集成，还必须检查并同步：

```text
mem_ut/ver/ut/memblock/env/src/memblock_env.sv
mem_ut/ver/ut/memblock/env/src/memblock_env_cfg.sv
mem_ut/ver/ut/memblock/env/src/memblock_rm.sv
mem_ut/ver/ut/memblock/common/memblock_common/src/*.sv
mem_ut/ver/ut/memblock/cfg/tb.f
mem_ut/ver/ut/memblock/tc/src/*.sv
```

典型触发条件：

- 新增或移除 agent
- monitor 输出事务类型变化
- reference model、scoreboard、coverage 使用了变化字段
- sequence 需要生成新增字段或更新 idx 合法范围
- cfg 需要新增控制开关

新增或重新添加 agent 时，同时遵循：

```text
mem_ut/ver/ut/memblock/rule/memblock_agent_add_rule.md
```

新增 cfg 或 user_ctrl 字段时，同时遵循：

```text
mem_ut/ver/ut/memblock/rule/memblock_cfg_add_rule.md
```

## 完成标准

完成最新 DUT 适配后，至少满足：

- `top_tb.sv` 展开的所有 RTL 交接 interface 已检查。
- `dut_inst.sv` 中 DUT 端口声明和实例连接与最新 Verilog RTL 一致。
- 每个变化接口对应的 `*_agent_connect.sv` 已同步。
- 每个变化 agent 的 interface、xaction、driver、monitor 已同步。
- 旧端口名、旧层级路径、旧 transaction 字段没有残留引用。
- valid/ready、idx、数组 channel、位宽相关约束已按最新 RTL 修正。
- 如影响 env/RM/sequence/cfg，相关文件已同步。

## 建议检查命令

从仓库根目录执行静态检查：

```bash
rg -n "旧端口名|旧层级路径|旧字段名" mem_ut/ver/ut/memblock build_memblock/rtl
rg -n 'MEMBLOCK_CONNECT|_AGENT_CONNECT|uvm_config_db#\(virtual' mem_ut/ver/ut/memblock/tb
git diff --check -- mem_ut/ver/ut/memblock
```

如涉及可编译代码变更，按远端 flow 验证：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
```

通过标准：

- 编译无 DUT 端口、层级路径、interface 字段、transaction 字段相关错误。
- 仿真看到 `TEST CASE PASSED`。
- `UVM_ERROR` 和 `UVM_FATAL` 均为 0。
