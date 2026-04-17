# Frontend BT 覆盖率能力评估与量化方案

## 1. 目的

本文回答三个问题：

1. 当前 Python Frontend BT 环境已经具备哪些覆盖率收集能力。
2. 当前哪些覆盖率指标已经能量化，哪些还没有真正接起来。
3. 当前环境是否具备获取 DUT 内部信号、用于功能覆盖率建模和其他覆盖率收集的能力。

本文只描述当前仓库中已经看得到的能力和缺口，不把工具链可能支持但仓库里没接上的能力写成“已有”。

## 2. 当前能力总览

| 维度 | 当前状态 | 当前量化产物 | 说明 |
| --- | --- | --- | --- |
| 波形观测 | 已具备 | `*.fst` | 通过 `dut.SetWaveform()` 生成 |
| 代码行覆盖率原始数据 | 已具备 | `*.dat` | 通过 `dut.SetCoverage()` 生成原始 coverage 数据 |
| 代码行覆盖率报告化 | 未接通 | 无稳定 report | 当前 Frontend env 未调用 `set_line_coverage()` |
| 功能覆盖率原始记录 | 已具备 | `*.funcov.json` | 当前主线已稳定产出 |
| 功能覆盖率汇总 | 已具备 | `*.funcov.summary.csv` | 按 group 汇总命中率 |
| 功能覆盖率未命中清单 | 已具备 | `*.funcov.unhit.csv` | 可直接反推缺口 |
| 行为统计指标 | 已具备 | `env.get_stats()` 写入 raw funcov | 包含 commit、PTW、气泡率、MPKI 等 |
| RTL branch coverage | 未集成 | 无 | 当前代码里没有 branch coverage report pipeline |
| RTL toggle coverage | 未集成 | 无 | 当前代码里没有 toggle coverage report pipeline |
| RTL FSM/state coverage | 未集成 | 无 | 当前代码里没有 FSM coverage report pipeline |
| DUT 已导出信号采样 | 已具备 | Python 直接读 pin.value | 前提是信号在生成 DUT 接口中可见 |
| DUT 任意内部寄存器采样 | 不具备稳定通路 | 无 | 除非该信号被导出到 DUT 对象或 debug 接口 |

## 3. 当前仓库里已经接上的量化能力

### 3.1 功能覆盖率

当前功能覆盖率是当前 Frontend BT 环境里最成熟的一块。

现有实现位置：

- `src/test/python/Frontend/env/functional_coverage.py`
- `src/test/python/Frontend/docs/frontend_bt_functional_coverage_pilot.csv`

当前已接通的输出：

1. `*.funcov.json`
   - 原始命中记录
   - 每个 bin 的 `hits / first_cycle / last_cycle / evidence`
   - 当前运行的 `stats`
   - 当前运行的 `errors`
2. `*.funcov.summary.csv`
   - 每个 `Coverage_Group` 的：
     - `Total_Bins`
     - `Hit_Bins`
     - `Coverage_Pct`
3. `*.funcov.unhit.csv`
   - 当前未命中的 bin 清单

这意味着：

1. 当前功能覆盖率已经能量化。
2. 当前已经有最基础的“表格化可视化”能力。
3. 当前缺的是更友好的 dashboard / HTML /趋势图，不是缺原始数据。

### 3.2 行为统计指标

当前 `FrontendEnv.get_stats()` 已经提供一组可量化运行指标，并被写入 raw funcov：

- `monitor`
  - `cycles_total`
  - `slots_total`
  - `slots_valid`
  - `bubble_ratio`
  - `avg_fetch_width`
  - `redirect_count`
  - `exception_mark_count`
  - `error_count`
- `icache`
  - `req_count`
  - `resp_beat_count`
  - `resp_line_count`
  - `miss_count`
  - `max_pending_depth`
- `uncache`
  - `req_count`
  - `resp_count`
- `ptw`
  - `req_count`
  - `resp_count`
  - `mode`
- `backend`
  - `commit_count`
  - `ftq_entries_pending`
  - `pending_resolves`
- `branch`
  - `total_instruction`
  - `total_branch`
  - `mispredict`
  - `mpki`
  - `by_type`

这些指标虽然不是传统 EDA 工具意义上的 code coverage，但已经能形成一套稳定的“行为统计量化”。

### 3.3 代码行覆盖率原始数据

当前 `src/test/python/Frontend/env/fixtures.py` 在创建 DUT 时已经调用：

```python
dut.SetCoverage(str(coverage))
```

因此当前 Frontend env 已经会产出：

- `*.dat`

也就是说：

1. 当前代码行覆盖率的原始收集钩子已经存在。
2. 当前问题不是“完全没有 line coverage”，而是还没有把原始 `.dat` 报告化、纳入稳定指标体系。

## 4. 当前没有真正接通的覆盖率项

### 4.1 代码行覆盖率报告化

仓库里的通用 guide 文档已经说明了标准做法：

- `dut.SetCoverage(...)`
- teardown 中再调用 `set_line_coverage(...)`

但当前 Frontend env 实际代码里：

1. 已调用 `dut.SetCoverage(...)`
2. 未调用 `set_line_coverage(...)`
3. 未生成 line coverage 汇总表或门禁报告

所以当前结论是：

- 有原始 line coverage 数据
- 但没有 Frontend 专用的 line coverage report pipeline

### 4.2 RTL branch / toggle / FSM coverage

当前 Frontend Python 环境代码里，没有看到以下东西已经接入：

1. branch coverage 汇总文件生成
2. toggle coverage 汇总文件生成
3. FSM / state coverage 汇总文件生成
4. 对应的 HTML / CSV / summary 输出

因此当前不能把这些说成已有能力。

需要特别区分：

- `BranchChecker` 的 `branch / jump / jump_indirect / mpki`
  - 这是前端指令流行为统计
  - 不是 RTL code branch coverage

## 5. 当前功能覆盖率到底覆盖了多少

当前可直接量化的功能覆盖率有两层：

### 5.1 单 testcase 级

每个 testcase 都会输出：

- `*.funcov.summary.csv`
- `*.funcov.unhit.csv`

这已经能回答：

1. 这个 testcase 命中了哪些 coverage group/bin
2. 这个 testcase 没打到哪些 bin

### 5.2 多 testcase 汇总级

`FunctionalCoverageRecorder.merge_raw_files(...)` 已具备多 raw json 合并能力。

说明当前只差一个 Frontend 文档侧认可的“批量 merge / dashboard 入口”，而不是缺底层能力。

所以当前最合理的量化记录方式是：

1. 以 `funcov.summary.csv` 记录当前批次覆盖率
2. 以 `funcov.unhit.csv` 记录当前缺口
3. 再配一份 Markdown 摘要，说明：
   - 本轮 batch 的 hit bin
   - 本轮 batch 的 unhit bin
   - 关键 evidence 文件

## 6. 当前有没有可视化的东西

### 6.1 当前已具备的可视化

1. 波形可视化
   - `*.fst`
   - 可转 `fsdb`
2. 表格可视化
   - `*.funcov.summary.csv`
   - `*.funcov.unhit.csv`
3. 运行日志可视化
   - `*.log`
4. Web UI
   - 当前更偏向运行态观测和交互，不是 coverage dashboard

### 6.2 当前缺失的可视化

1. line coverage 百分比趋势图
2. funcov group 覆盖率趋势图
3. unhit bin 热点排行
4. 分 testcase / 分 batch 的统一 HTML 报告页
5. toggle / FSM / code branch 的统一覆盖率页面

## 7. 当前 Python 环境是否具备 DUT 内部信号获取能力

答案是：

### 7.1 对“已导出到 DUT 对象的信号”

具备。

当前环境里至少有三种读取方式：

1. 直接 `getattr(dut, signal_name).value`
2. bundle 绑定
   - `bind_bundle_required(...)`
   - `bind_bundle_optional(...)`
3. coverage / monitor 里的 `_read_dut_signal(...)`

当前仓库里已经在读取的信号例子包括：

- `io_backend_cfVec_*`
- `io_ptw_req_0_*`
- `io_ptw_resp_*`
- `io_frontendInfo_ibufFull`
- `io_backend_toFtq_redirect_*`

### 7.2 对“任意 RTL 内部隐藏寄存器/状态机”

当前不具备稳定通路。

当前 Python 环境能读到什么，前提是：

1. 该信号已经出现在生成 DUT 接口里
2. 或该信号通过当前 bundle / wrapper / debug 导出到 DUT 对象

如果某个内部状态没有出现在 DUT 接口或生成的 `signals.json` / `Frontend_top.sv` 中，就不能把它当成当前稳定可采样对象。

### 7.3 当前最准确的结论

当前环境具备：

- 读取 DUT 已导出信号
- 基于这些信号做功能覆盖率建模
- 基于这些信号做 monitor / checker /行为统计

当前环境不具备：

- 随意读取所有 RTL 内部私有状态
- 在没有导出 pin / debug 信号的前提下直接做内部状态机覆盖

## 8. 对 line / branch / toggle / FSM coverage 的建议口径

当前建议对外统一说：

1. **功能覆盖率**
   - 已接通
   - 已有 raw / summary / unhit 三类量化产物
2. **行为统计**
   - 已接通
   - 已有气泡率、平均取指宽度、MPKI、PTW 请求数等指标
3. **代码行覆盖率**
   - 原始 `.dat` 已能生成
   - 但 Frontend 专用的报告化 pipeline 还没接完
4. **branch / toggle / FSM coverage**
   - 当前 Python Frontend env 代码侧尚未形成稳定采集与汇总链路
   - 不能直接等同于 sv-uvm 环境下的成熟 EDA 覆盖率工具链

## 9. 当前建议的量化记录方案

如果现在就要给前端 BT 建一套“能持续记录、能汇报”的量化体系，建议分两层：

### 9.1 当前立刻可落地

1. 功能覆盖率
   - 记录 `funcov.summary.csv`
   - 记录 `funcov.unhit.csv`
2. 行为统计
   - 从 raw funcov 的 `stats` 提取：
     - `commit_count`
     - `bubble_ratio`
     - `avg_fetch_width`
     - `mpki`
     - `ptw.req_count`
     - `redirect_count`
3. 证据文件
   - 记录对应 `.fst / .dat / .funcov.json`

### 9.2 下一阶段建议补齐

1. 在 Frontend fixture 中接入 line coverage report pipeline
2. 增加多 testcase funcov merge 的标准入口
3. 产出统一 Markdown / CSV dashboard
4. 再评估底层工具链是否支持：
   - RTL branch coverage
   - toggle coverage
   - FSM coverage

## 10. 当前结论

一句话总结：

当前 Python Frontend BT 环境已经具备：

- 功能覆盖率量化
- 行为统计量化
- line coverage 原始数据生成
- DUT 已导出信号采样

但当前还没有形成与传统 sv-uvm 工具链等价的：

- line coverage 稳定报告化
- code branch coverage
- toggle coverage
- FSM coverage

这部分要么需要继续接 Frontend 专用 reporter，要么需要额外确认底层 DUT/仿真工具链支持并补接收集流水线。
