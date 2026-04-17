# T07 功能覆盖率扩展设计

## 1. 目标

在现有 `FG-BASIC/FG-API/FG-ICACHE/FG-PTW/FG-SFENCE/FG-FENCEI/FG-RESET` 基础上，新增前端黑盒关键覆盖组。

新增覆盖组：

- `FG-FETCH`
- `FG-BRANCH`
- `FG-REDIRECT`
- `FG-EXCEPTION`
- `FG-PERFORMANCE`

## 2. 覆盖组设计

## 2.1 FG-FETCH

功能点示例：

- `FC-FETCH-WIDTH`：每周期有效槽位 1~8
- `FC-FETCH-CROSSLINE`：跨 64B cacheline
- `FC-FETCH-RVC`：16b 指令路径

## 2.2 FG-BRANCH

功能点示例：

- `FC-BR-TYPE`：cond/jal/jalr/call/ret
- `FC-BR-PRED`：predTaken 组合
- `FC-BR-MISPRED`：方向错/目标错

## 2.3 FG-REDIRECT

功能点示例：

- `FC-RDR-SOURCE`：branch/exception/interrupt/watchdog
- `FC-RDR-DEPTH`：单次/嵌套 redirect
- `FC-RDR-LATENCY`：触发到恢复周期分布

## 2.4 FG-EXCEPTION

功能点示例：

- `FC-EXC-TYPE`：illegal/page fault/access fault
- `FC-EXC-RESP`：异常到 redirect 周期

## 2.5 FG-PERFORMANCE

功能点示例：

- `FC-PERF-MISSRATE`：低/中/高缺失率
- `FC-PERF-BUBBLE`：气泡率区间
- `FC-PERF-BW`：平均取指宽度区间
- `FC-PERF-MPKI`：MPKI 阈值分档

## 3. 实现方式

遵循现有 `toffee.funcov.CovGroup` 模式：

- 在 fixture 中通过 `dut.StepRis(lambda _: [g.sample() for g in groups])` 自动采样。
- `add_watch_point(dut, bins, name=...)` 定义功能点。
- `mark_function` 将关键测试映射到功能点。

## 4. 数据来源

- 总线握手：来自 ICache/Uncache/PTW 接口。
- 指令流行为：来自 `FrontendMonitor` 统计。
- 分支/重定向：来自 `BranchChecker` 与 `BackendModel` 事件。

## 5. 通过标准

- `FG-FETCH` 覆盖率 >= 90%
- `FG-BRANCH` 覆盖率 >= 90%
- 其余新增组覆盖率在回归中持续上升并无空组。
