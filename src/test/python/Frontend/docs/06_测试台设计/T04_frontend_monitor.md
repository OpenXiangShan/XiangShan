# T04 FrontendMonitor 设计

## 1. 目标

`FrontendMonitor` 负责每周期观察 `io_backend_cfVec_{0..7}_*`，提供：

- PC 序列一致性检查
- 指令编码一致性检查（对照 `MemoryModel`）
- 带宽/气泡统计
- 结构化错误上报

## 2. 输入信号

最小采样集：

- `io_backend_cfVec_i_valid`
- `io_backend_cfVec_i_bits_pc`
- `io_backend_cfVec_i_bits_instr`
- `io_backend_cfVec_i_bits_isRvc`
- `io_backend_cfVec_i_bits_predTaken`
- `io_backend_cfVec_i_bits_exceptionVec_*`（异常标记）

## 3. 核心接口

- `on_clock_edge(cycle: int) -> None`
- `set_expected_pc(pc: int) -> None`
- `notify_redirect(target_pc: int, reason: str) -> None`
- `get_errors() -> list[dict]`
- `get_stats() -> dict`
- `clear() -> None`

## 4. PC 连续性状态机

状态：

- `RESET_WAIT`
- `SEQUENTIAL`
- `REDIRECT_PENDING`
- `EXCEPTION_PENDING`

规则：

1. 顺序态下，`next_pc = pc + (2 if isRvc else 4)`。
2. 收到 redirect 通知后，将 `expected_pc` 切到 `target_pc`。
3. 允许一个可配置宽限窗口（默认 1~2 cycle）吸收管线反应延迟。
4. 超窗仍不匹配则记 `PC_MISMATCH`。

## 5. 指令内容比对

对每个 `valid` 槽位：

- 从 `MemoryModel.read_u32(pc)` 读取期望编码（RVC 场景读取低 16b 并扩展比较）。
- 若与 `bits_instr` 不一致，记录 `INSTR_MISMATCH`。

错误记录字段：

- `cycle`, `slot`, `pc`, `expected`, `actual`, `kind`, `context`

## 6. 性能统计

建议统计：

- `cycles_total`
- `slots_total = cycles_total * 8`
- `slots_valid`
- `bubble_ratio = 1 - slots_valid / slots_total`
- `avg_fetch_width = slots_valid / cycles_total`
- `redirect_count`
- `exception_mark_count`

## 7. 与 BackendModel 协同

- `BackendModel` 注入 redirect/exception 时调用 `notify_redirect()`。
- `FrontendMonitor` 输出分支相关观测给 `BranchChecker`。

## 8. 验收标准

1. 可识别 PC 跳跃、回退、错位三类问题。
2. 可统计带宽与气泡率，并在 1000 周期内稳定输出。
3. Redirect 生效后，监控器能在允许窗口内观测到目标 PC。
