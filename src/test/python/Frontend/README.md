# Frontend Python Verification Environment

`src/test/python/Frontend/` 是面向 Frontend 真实 DUT 的 Python 验证环境目录。

这里和 `MemBlock/` 的组织目标一致: 根目录提供稳定入口，真实验证实现放在职责清晰的子目录中。当前 Frontend 的主要差异是，toffee 环境主体已经沉到 `env/`，因此根目录更多承担 facade 角色，而不是再复制一层实现。

## 目录说明

### 根目录

- `Frontend_api.py`
  - 顶层 DUT/API 入口。
  - 暴露 `create_dut`、`dut` 以及公共 `api_Frontend_*` helper。
- `Frontend_env.py`
  - 顶层环境入口。
  - 暴露 `FrontendEnv`、`env`、`full_env`。
- `conftest.py`
  - 只负责路径初始化和 `data/` 目录准备。
- `README.md`
  - 当前文件，作为 Frontend Python 验证目录入口说明。
- `run_pytest_with_log.sh`
  - pytest 运行脚本。
- `run_web_console.sh`
  - Web UI 启动脚本。
- `run_dut_with_bin_trace.py`
  - 结合程序镜像和 golden trace 的 DUT 运行入口。
- `run_bin_trace_pipeline.sh`
  - bin + NEMU trace 联动脚本。
- `fst_to_fsdb.sh`
  - 将 `.fst` 波形转换为 `.fsdb` 的脚本。
- `nemu_bin_to_golden_trace.py`
  - 从二进制触发 NEMU trace 生成。
- `nemu_log_to_golden_trace.py`
  - 从 NEMU 日志转换 golden trace。

### 子目录

- `env/`
  - Frontend toffee 验证环境主实现层。
  - 包含 bundles、agents、monitors、model、sequences、fixtures 和 API。
- `tests/`
  - 当前可见的 Frontend pytest 用例集合。
- `webui/`
  - Web UI 服务端与静态资源。
- `data/`
  - 波形、覆盖率和测试产物目录。

## 分层约定

- 根目录负责稳定导入入口，不重复实现 `env/` 内逻辑。
- `env/fixtures.py` 是 DUT fixture 与环境装配的真实实现位置。
- `env/api.py` 是公共 `api_Frontend_*` helper 的真实实现位置。
- `env/frontend_env.py` 是 `FrontendEnv` 的真实实现位置。
- `env/bundles/`、coverage 和启动控制里出现的信号名，必须以当前生成出来的 DUT 接口为准。
  不允许长期保留已经不在 DUT 中出现的历史信号；缺失信号要么从 bundle/coverage 中删除，要么被明确建模为可选信号。

## 阅读建议

如果是首次接触当前环境，建议按以下顺序阅读：

1. `README.md`
2. `Frontend_api.py`
3. `Frontend_env.py`
4. `env/frontend_env.py`
5. `env/api.py`
6. `env/fixtures.py`
7. `tests/test_layout_import_compat.py`
8. `tests/test_bin_trace_dut.py`
9. `tests/test_multi_branch.py`

## 常用脚本

- `fst_to_fsdb.sh`
  - 用法: `src/test/python/Frontend/fst_to_fsdb.sh <input.fst> [output.fsdb]`
  - 若不传 `output.fsdb`，默认在输入文件同目录下生成同名 `.fsdb`
  - 中间 `.vcd` 放在临时目录，脚本结束后自动清理

## Bin Case 标准入口

- `microbench.bin` 的标准运行入口不是裸 `pytest`，而是：

```bash
source /nfs/share/unitychip/activate
source /nfs/home/zhaoxinran/.venv/mcpgateway/bin/activate
TB_NEMU_EXEC=ready-to-run/riscv64-nemu-interpreter \
TB_ENV_LOG_LEVEL=INFO \
TB_TRACE_PROGRESS_INTERVAL=50000 \
TB_TRACE_STALL_SNAPSHOT_INTERVAL=5000 \
TB_TRACE_STAGNANT_CYCLES_LIMIT=20000 \
TB_PYTEST_TIMEOUT_SECS=900 \
PYTEST_ADDOPTS='-s -o log_cli=true --log-cli-level=INFO' \
src/test/python/Frontend/run_bin_trace_pipeline.sh ready-to-run/microbench.bin
```

- 上面这条命令同时完成：
  - 从 bin 生成 NEMU trace
  - 设置 `test_bin_trace_dut.py::test_bin_trace` 所需的 pipeline 环境变量
  - 打开 progress / stall 观测输出
  - 生成 DUT `.fst` 和配套 `.log`
- 只有在 trace 已经准备好，并且以下环境变量都已显式设置时，才允许直接运行 `pytest`：

```bash
TB_ENABLE_DUT_TESTS=1 \
TB_BIN_TRACE_PIPELINE=1 \
TB_BIN_PATH=ready-to-run/microbench.bin \
TB_TRACE_PATH=NEMU/logs/microbench.trace.jsonl \
TB_BASE_ADDR=0x80000000 \
TB_STEP_CYCLES=0 \
TB_TRACE_STAGNANT_CYCLES_LIMIT=20000 \
TB_RUN_TO_TRACE_COMPLETION=1 \
python -m pytest -v src/test/python/Frontend/tests/test_bin_trace_dut.py::test_bin_trace
```

- 当前仓库里实际存在的 NEMU 可执行文件路径是
  `ready-to-run/riscv64-nemu-interpreter`。
  不要默认假设 `NEMU/build/riscv64-nemu-interpreter` 已经存在。

## Bin Case 运行要求

- `tests/test_bin_trace_dut.py::test_bin_trace` 这类 bin case，每次运行都必须生成：
  - 一份 `.fst` 波形
  - 一份配套 `.log` 日志
- 默认应优先落到 `src/test/python/Frontend/data/<YYYYMMDD>/` 这样的日期目录下。
- bin case 运行时必须有明确的观测机制，例如：
  - progress checkpoint
  - stall snapshot
  - 或等价的运行时诊断输出
- 不接受“卡死后一直无输出、只能靠外部 timeout/手动中断结束”的 bin case 运行方式。
- 统一要求设置 `TB_TRACE_STAGNANT_CYCLES_LIMIT`，当 golden cursor 长时间不前进时，
  在测试内部早停并失败（优先于外部 kill）。
