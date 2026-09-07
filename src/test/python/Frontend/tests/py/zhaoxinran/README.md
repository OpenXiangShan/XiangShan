# Zhaoxinran Frontend 测试目录

## 结论

本目录已按稳定的被测机制拆为 `translation/`、`uncache/` 和 `mmio/` 三组。
跨域测试和稳定入口继续留在根目录，不建立只有单个入口的 `pipeline/`、
`cacheable/` 或薄 `control_flow/` 目录。

公共场景代码放在同级的 `tests/py/support/`，不再让大量测试文件依赖某一个
作者目录下的测试模块。此次只抽取已经存在跨文件复用的代码，没有建立笼统的
`common.py`，也没有改变测试断言、marker 或 DUT gate。

## 当前目录

```text
tests/py/
├── support/
│   ├── __init__.py
│   ├── translation_faults.py
│   └── uncache_scenarios.py
└── zhaoxinran/
    ├── translation/
    │   ├── __init__.py
    │   ├── test_address_translation_context_switch.py
    │   ├── test_address_translation_fault.py
    │   ├── test_address_translation_gstage_provenance.py
    │   ├── test_address_translation_normal.py
    │   ├── test_address_translation_pte_permission.py
    │   ├── test_address_translation_ptw_timing.py
    │   ├── test_instruction_fetch_permission_boundary.py
    │   └── test_translation_random_regression_dut.py
    ├── uncache/
    │   ├── __init__.py
    │   ├── test_instr_uncache_port_boundaries.py
    │   └── test_nc_fetch_paths.py
    ├── mmio/
    │   ├── __init__.py
    │   ├── test_mmio_fetch_attributes.py
    │   ├── test_mmio_fetch_boundary.py
    │   ├── test_mmio_fetch_control_flow.py
    │   ├── test_mmio_fetch_flow_control.py
    │   ├── test_mmio_fetch_flush.py
    │   ├── test_mmio_fetch_page_state.py
    │   ├── test_mmio_fetch_response_handoff.py
    │   ├── test_mmio_fetch_state_edges.py
    │   └── test_mmio_fetch_translation_faults.py
    ├── test_bin_trace_dut.py
    ├── test_frontend_exception_paths.py
    ├── test_multi_branch.py
    └── test_redirect_flush_recovery_scenarios.py
```

现有 `src/test/python/Frontend/tests/conftest.py` 对这些后代目录继续生效，子目录
不复制 `conftest.py`。

## 分组依据

| 位置 | 文件数 | 展开用例数 | 范围 |
| --- | ---: | ---: | --- |
| `translation/` | 8 | 132 | 地址翻译、PTE 权限、PTW 时序、上下文切换和随机回归 |
| `uncache/` | 2 | 88 | InstrUncache 端口边界和 PBMT.NC 路径 |
| `mmio/` | 9 | 53 | MMIO 属性、边界、控制流、流控、flush 和 fault |
| 根目录 | 4 | 34 | bin-trace、cacheable exception、multi-branch 和跨域恢复 |
| 合计 | 23 | 307 | 不含 `__init__.py` |

分组以数据路径或机制为主，不单独建立 `exceptions/`。translation、MMIO、NC
和 cacheable 路径都包含异常行为，按异常类型拆分会把同一场景的正常流、边界流
和 fault 流分散到不同目录。也不按 DUT/non-DUT 分目录，因为同一个文件可能同时
包含两类测试，运行属性不是稳定的文件边界。

根目录中的四个文件有意保持不动：

- `test_bin_trace_dut.py` 是 shell runner 和契约测试使用的稳定入口；增加
  `pipeline/` 只有路径成本，没有分组收益。
- `test_frontend_exception_paths.py` 是单个 cacheable exception 聚合文件；单独
  建立 `cacheable/` 过薄。
- `test_multi_branch.py` 是跨作者复用的控制流测试模块。
- `test_redirect_flush_recovery_scenarios.py` 横跨 translation、MMIO、uncache、
  BPU 和 ICache，保留根目录更能表达其跨域性质。

## 公共 support

### `support/uncache_scenarios.py`

该模块承接原 `test_instr_uncache_port_boundaries.py` 中已被多个文件复用的场景
准备、驱动、等待、地址属性配置和相关常量，主要包括：

- MMIO、cross-beat、cross-page、SV39 PBMT.NC 程序准备；
- fetch 初始化、redirect、sfence 和 DUT 请求/响应等待；
- PMP/PMA 配置；
- uncache 地址、指令、PBMT 和状态常量。

`_RUN_DUT` 仍由各测试模块本地读取 `TB_ENABLE_DUT_TESTS`，避免某个 support
模块在 import 时替其他测试决定 pytest gate。已经属于 `env` 的类型由消费者
直接从 `env.sequences`、`env.core.transactions` 或 `env.support` 导入，support
不做二次转发。

原先被其他测试直接调用的完整 `test_*` 已拆为稳定 pytest 入口和普通 `_run_*`
场景主体，包括六个 uncache 场景以及跨域恢复复用的 translation、MMIO、ICache
场景。pytest marker 仍挂在 `test_*` 入口上，跨文件复用只调用场景主体，因此测试
函数本身不再作为公共 API。

### `support/translation_faults.py`

该模块承接 translation、MMIO 和 NC 测试共同使用的：

- cross-page fault case matrix；
- exception bit 映射；
- cfVec/gpaddr 采样；
- fault PC、exception type、cross-page 和 FTQ identity 公共断言。

具体 pytest 测试、单文件场景构造和各文件自己的期望仍留在对应测试文件中。

### 暂不抽取

- `test_nc_fetch_paths.py` 目前只向一个 MMIO 文件提供两个 helper，继续放在
  `uncache/`；出现更多独立消费者后再考虑 support API。
- `test_multi_branch.py` 和 `test_redirect_flush_recovery_scenarios.py` 是完整场景或
  跨域集成测试，不作为公共 helper 集合。
- 其他作者目录中仅供单个文件内部使用的代码，本次不扩大抽取范围。

## 迁移兼容性

- pytest 会递归收集三个子包，父级 fixture 不受影响。
- Python 调用方已更新到新的模块路径，没有保留会重复收集测试的旧路径 wrapper。
- `test_bin_trace_dut.py::test_bin_trace` 路径未变，bin-trace runner 契约不受影响。
- 文件路径变化会改变完整 pytest nodeid，因此旧 `--lf` 缓存、run manifest 和
  coverage sidecar 中的 testcase identity 只能作为历史证据；测试函数名和参数 ID
  集合保持不变。

## 验证结果

2026-09-07 在仓库根目录完成以下验证：

- 移动前后按 `test_filename::test_function[param-id]` 规范化比较：307 项完全一致，
  无新增、无缺失；
- `zhaoxinran/` collect-only：`307 tests collected`；
- 整个 `tests/py/` collect-only：`1162 tests collected`；
- 默认未启用 DUT 的 `zhaoxinran/` 执行：`3 passed, 304 skipped`；
- layout import 与 bin-trace runner 契约：`10 passed`；
- Frontend change guard：`76 passed`；
- Verilator DUT-enabled `zhaoxinran/` 完整回归：
  `290 passed, 15 failed, 2 skipped in 5003.74s`。

默认执行中的 304 个 skip 来自未设置 `TB_ENABLE_DUT_TESTS=1`，所以这些结果只证明
目录、导入、收集和非 DUT 用例正常。DUT-enabled 回归已经到达完整 pytest final
summary，但存在 15 个真实失败，因此不能宣称 DUT 回归通过。失败主要涉及缺少
内部观测信号、translation fault PC/异常交付、fault 后继续 fetch，以及 uncache
redirect/pending 状态；本次目录重构没有使用 skip、xfail 或弱化 checker 掩盖。

DUT 回归的两个 skip 分别是 pipeline-only 的 bin-trace 入口，以及当前生成 DUT
未暴露 `io_tlbCsr_mPBMTE` 的 PBMT.IO 用例。主日志为
`src/test/python/Frontend/logs/pytest_20260907_112656.log`，逐用例 FST、coverage 和
case log 位于 run `frontend_pytest_20260907_112659_616821_1552113`；这些运行产物
不纳入源码提交。
