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
- `scripts/`
  - Frontend 目录下的 shell 脚本入口。
  - 包含 `run_pytest_with_log.sh`、`run_web_console.sh`、
    `run_bin_trace_pipeline.sh`、`fst_to_fsdb.sh`、
    `gen_coverage_html.sh` 和 `report_raw_code_coverage.py`。
- `tools/`
  - Frontend 目录下的 Python 工具入口。
  - 包含 `run_dut_with_bin_trace.py`、
    `nemu_bin_to_golden_trace.py` 和
    `nemu_log_to_golden_trace.py`。

## 分层约定

- 根目录负责稳定导入入口，不重复实现 `env/` 内逻辑。
- `env/fixtures.py` 是 DUT fixture 与环境装配的真实实现位置。
- `env/api.py` 是公共 `api_Frontend_*` helper 的真实实现位置。
- `env/request_apis.py` 是 `env/api.py` 下层请求式 helper 的真实实现位置。
- `env/frontend_env.py` 是 `FrontendEnv` 的真实实现位置。
- `env/dut_factory.py` 负责真实 DUT 构造。
- `env/nemu_trace_pipeline.py` 负责从 bin 驱动 NEMU trace 生成。
- `env/functional_coverage.py` 负责功能覆盖率事件记录与产物输出。
- `env/monitor.py` 与 `env/monitors/` 共同承担 monitor 侧数据结构和 DUT 观测逻辑。
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

- `scripts/fst_to_fsdb.sh`
  - 用法: `src/test/python/Frontend/scripts/fst_to_fsdb.sh <input.fst> [output.fsdb]`
  - 若不传 `output.fsdb`，默认在输入文件同目录下生成同名 `.fsdb`
  - 用于 FST 波形；frontend 默认构建产物是 `.fst`
  - 若显式执行 `make verilog FRONTEND_WAVEFORM_FORMAT=vcd` 或 `make frontend FRONTEND_WAVEFORM_FORMAT=vcd`，则 frontend pylib 会改为生成 `.vcd`
  - 一旦 `build-frontend/.waveform_format` 已记录为 `vcd`，后续不带参数的 `make frontend` 会沿用 `vcd`；只有显式指定 `FRONTEND_WAVEFORM_FORMAT=fst` 才会切回 `.fst`
  - 中间 `.vcd` 放在临时目录，脚本结束后自动清理
- `scripts/gen_coverage_html.sh`
  - 用法: `src/test/python/Frontend/scripts/gen_coverage_html.sh [input.dat ... | input_dir] [output_dir]`
  - 不传输入时，默认收集 `data/*.dat`
  - 单个 `.dat` 默认输出到同目录下的 `<stem>.genhtml/`
  - 多个 `.dat` 或目录输入默认输出到 `coverage.genhtml/`
  - 会自动生成 `merged.info` 并调用 `genhtml --ignore-errors range --filter missing`
  - 若要把指定 `.dat` 合并到已有 `coverage.genhtml/`，可直接执行：
    `source /nfs/share/unitychip/activate && PATH=/nfs/share/unitychip/bin:$PATH src/test/python/Frontend/scripts/gen_coverage_html.sh src/test/python/Frontend/data/<case>.dat src/test/python/Frontend/data/coverage.genhtml`
- `scripts/report_raw_code_coverage.py`
  - 用法: `python src/test/python/Frontend/scripts/report_raw_code_coverage.py`
  - 直接合并 `data/*.dat`，按 raw 覆盖点输出总 `line/branch/expr/toggle` 覆盖率
  - 同时给出 `ifu_strict`、`ifu_core`、`icache`、`bpu`、`tlb_pmp`、`fault_path` 的 raw line 覆盖率拆分
- `scripts/asm_to_jsonl.sh`
  - 用法: `src/test/python/Frontend/scripts/asm_to_jsonl.sh <case.S> [bin_path] [trace_jsonl_path]`
  - 默认把 `.S` 链接到 `0x10001000`，按 NEMU memory base `0x10000000`
    在最终 `.bin` 前补 `0x1000` 字节 0
  - 默认输出 `.bin` 到 `tests/asm_cases/generated/<case>.bin`，输出 golden trace 到
    `NEMU/logs/<case>.trace.jsonl`
  - 通过现有 `tools/nemu_bin_to_golden_trace.py` 调用 NEMU 并转换 trace；
    raw NEMU log 默认写到 `NEMU/logs/<case>.nemu.log`
  - 调用前需要先激活 frontend Python/runtime 环境；脚本本身不写死 venv 路径
  - 可用 `NEMU_EXEC=/path/to/riscv64-nemu-interpreter` 手动指定 NEMU；
    默认使用 `ready-to-run/riscv64-nemu-interpreter`
  - 最终 `.bin` 已经是 NEMU 可跑格式，不生成 `_padded` 文件名

## 常用工具

- `tools/nemu_bin_to_golden_trace.py`
  - 路径: `src/test/python/Frontend/tools/nemu_bin_to_golden_trace.py`
- `tools/nemu_log_to_golden_trace.py`
  - 路径: `src/test/python/Frontend/tools/nemu_log_to_golden_trace.py`
- `tools/run_dut_with_bin_trace.py`
  - 路径: `src/test/python/Frontend/tools/run_dut_with_bin_trace.py`

## 运行入口

- 默认回归入口：

```bash
src/test/python/Frontend/scripts/run_pytest_with_log.sh
```

- bin trace 标准入口：

```bash
src/test/python/Frontend/scripts/run_bin_trace_pipeline.sh ready-to-run/<case>.bin
```

- 如果需要在指定 `cursor` 达到时提前通过，可额外设置
  `TB_TRACE_TARGET_CURSOR=<index>`。

- 详细测试参数、bin-trace 环境变量、runtime bound、artifact 规则和
  direct `pytest` 约束，统一见 `docs/agents/frontend-verification.md`。

## 文档分工

- 目录结构、入口分层和当前实现定位，以当前 README 为准。
- frontend 验证流程、bin-trace 运行要求、artifact 规则和提交约束，统一见
  `docs/agents/frontend-verification.md`。
- DUT / monitor / env mismatch 的分析方法，统一见
  `docs/agents/frontend-debugging.md`。
