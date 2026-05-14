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

当前主线同时维护 Python 侧事务/行为功能覆盖率。为降低 RTL 变动对建模的冲击，BPU_FTQ 旧功能覆盖率迁移先收敛到 `src/test/python/Frontend/env/funcov/`：

- `funcov/bpu.py`：放从旧 `BPU_FTQ_funcov.sv` 映射到当前 `Frontend.inner_bpu` 的 BPU 覆盖项
- `funcov/backend.py`：放从旧 backend redirect covergroup 映射到顶层 redirect 输入的覆盖项
- `funcov/ftq.py`：放 FTQ 指针/边界等稳定覆盖项

建模原则：

- 优先使用 `Frontend top` 已导出、语义稳定的信号
- BPU 覆盖项优先采 `Frontend.inner_bpu` 内部信号，不使用 `cfVec` 归因到 BPU
- BPU 内部信号采样必须有明确门控：输出链路用 `prediction.valid && prediction.ready`，s3 子预测器用 `s3_valid && prediction.ready`，uBTB 用自身 `s1_fire`
- 当旧 `BPU_FTQ_funcov.sv` 中的 RTL 内部信号不存在或语义漂移较大时，先保留可映射的顶层行为覆盖，不强行补历史内部 bins

## 4. 数据来源

- 总线握手：来自 ICache/Uncache/PTW 接口。
- 指令流行为：来自 `FrontendMonitor` 统计。
- 分支/重定向：来自 `BranchChecker` 与 `BackendModel` 事件。

## 5. 通过标准

- `FG-FETCH` 覆盖率 >= 90%
- `FG-BRANCH` 覆盖率 >= 90%
- 其余新增组覆盖率在回归中持续上升并无空组。
