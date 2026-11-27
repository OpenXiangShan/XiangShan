# Zhaoxinran Frontend 测试目录

## 目录与入口

| 位置 | 范围 |
| --- | --- |
| `translation/` | 地址翻译、权限、PTW 时序、上下文切换和随机场景 |
| `uncache/` | InstrUncache 端口边界和 PBMT.NC 路径 |
| `mmio/` | MMIO 属性、边界、控制流、流控、flush 和 fault |
| 根目录 | bin-trace、控制流、异常、跨域恢复、VType 及相关单测 |
| `support/` | 与本目录场景耦合的准备和观察 helper |
| `../support/` | 跨目录复用的 DUT、翻译和 uncache helper |

父级 `tests/conftest.py` 为这些目录提供 fixture。
`test_bin_trace_dut.py::test_bin_trace` 是 shell runner 使用的入口。
用例和参数以当前 pytest 收集结果为准。

## Helper 维护

优先复用 `env/` 和 `../support/` 中的已有能力。
`dut_support.py` 提供通用信号查找和有界等待，`translation_scenarios.py`、
`translation_faults.py` 和 `uncache_scenarios.py` 提供对应场景的公共准备与检查。
具体 pytest 测试、场景期望及单文件 helper 保留在所属模块。
跨文件复用使用普通场景函数，pytest marker 放在测试入口上。

测试模块本地读取 `TB_ENABLE_DUT_TESTS`，保持各入口的 DUT gate。
移动或复用 helper 时核对返回值、随机调用顺序、reset/时钟副作用、observer
注册顺序、采样阶段、超时和断言语义。

## 可复现随机回归

在仓库根目录运行 smoke 或 nightly。runner 调用
`run_pytest_with_log.sh`，启用 DUT 测试，并为日志和运行产物分配独立目录。
nightly 会继续执行剩余 seed，任一 seed 失败时最终返回非零。

```bash
src/test/python/Frontend/scripts/run_random_regression_suite.sh smoke
src/test/python/Frontend/scripts/run_random_regression_suite.sh nightly
```

- `TB_SEED` 控制使用 `scenario_rng()` 的场景，seed 由 base seed、scenario key
  和 ordinal 稳定派生。
- `TB_RANDOM_NIGHTLY_SEEDS` 指定 nightly 的 seed 列表，选择和默认值见 runner
  的 `--help`。
- translation stream 使用独立的 `TB_TRANSLATION_RANDOM_SEED`，以及
  `TB_TRANSLATION_RANDOM_COUNT` 和 `TB_TRANSLATION_RANDOM_START_ORDINAL`。
  runner 的 nightly seed 列表只传给 `TB_SEED`，translation seed 需单独设置。
- `env/support/random_regression.py` 的 `record_scenario()` 记录派生 seed、
  ordinal、参数哈希和用例提供的参数。fixture 将这些记录纳入运行 metadata。

随机化时保留会改变异常类型、fault PC、请求顺序、跨 beat/跨页性质或恢复行为
的各个语义场景。场景内的地址、载荷和延迟可由 seed 派生，修改前后逐项核对
触发条件和可观察预期。

## 合并后的场景入口

共用同一激励流程和检查项的用例集中到以下入口；语义参数逐项执行，地址和
延迟继续由已有 seed 派生。首拍 denied-only 的 resend 行为单独保留。

| 场景 | 保留入口 | 合并的原入口 |
| --- | --- | --- |
| 跨 8B 正常 RVI | `mmio/test_mmio_fetch_boundary.py::test_mmio_cross_8b_second_beat_response_modes[None]` | `test_mmio_cross_8b_clean_rvi_requests_next_beat_and_delivers` |
| 跨 8B 首拍 fault | `mmio/test_mmio_fetch_boundary.py::test_mmio_cross_8b_first_beat_fault_reports_without_resend` | `test_uncache_resend_first_beat_corrupt_suppresses_resend`、`test_uncache_resend_first_beat_combined_fault_suppresses_resend` |
| 页尾 RVI 拼接、半指令保持及 redirect 恢复 | `uncache/test_instr_uncache_port_boundaries.py::test_uncache_page_tail_rvi_need_resend_rechecks_next_page` | `test_mmio_page_tail_rvi_rechecks_next_page_before_delivery`、`test_mmio_page_tail_rvi_preserves_half_pc_data_and_state` |
| 非 MMIO 的 PBMT.NC 取指 | `uncache/test_instr_uncache_port_boundaries.py::test_uncache_pbmt_nc_non_mmio_uses_uncache_path` | `test_pbmt_nc_non_mmio_enters_uncache_send_path` |
| 后端取指异常重定向及恢复 | `test_redirect_flush_recovery_scenarios.py::test_rfr_s01_fault_redirect_recovery_three_fetches` | `test_backend_fault_redirect_recovery` |
