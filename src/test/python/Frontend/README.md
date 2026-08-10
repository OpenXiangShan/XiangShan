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
  - 负责路径初始化、`data/` 目录准备，以及 VCS batch pytest 的 session
    级 DUT finalization。
- `README.md`
  - 当前文件，作为 Frontend Python 验证目录入口说明。

### 子目录

- `env/`
  - Frontend toffee 验证环境主实现层。
  - 包含 bundles、agents、monitors、model、sequences、fixtures 和 API。
- `tests/`
  - 当前可见的 Frontend pytest 用例集合。
  - `tests/conftest.py`、`tests/asm_cases/generate_cases.py` 和
    `tests/asm_cases/generated/` 保持共享。
- `webui/`
  - Web UI 服务端与静态资源。
- `data/`
  - 波形、覆盖率和测试产物目录。
- `scripts/`
  - Frontend 目录下的 shell 脚本入口。
  - 包含 `run_pytest_with_log.sh`、`run_web_console.sh`、
    `run_bin_trace_pipeline.sh`、`run_bin_trace_suite.sh`、`fst_to_fsdb.sh`、
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
- `env/funcov/__init__.py` 负责通用 canonical group/coverpoint/bin 的 DUT 周期级采样，
  `env/funcov/py/icache/` 下的
  `icache_mainpipe_funcov.py`、`icache_prefetchpipe_funcov.py`、
  `icache_missunit_funcov.py`、`icache_waylookup_funcov.py` 和
  `icache_hitmiss_funcov.py` 分别负责 ICache MainPipe、PrefetchPipe、
  MissUnit、WayLookup 与 hit/miss 路径功能覆盖率模型；
  不再维护平行的 toffee 功能覆盖率定义。
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
7. `tests/py/environment/test_layout_import_compat.py`
8. `tests/py/zhaoxinran/test_bin_trace_dut.py`
9. `tests/py/zhaoxinran/test_multi_branch.py`

## 常用脚本

- `scripts/fst_to_fsdb.sh`
  - 用法: `src/test/python/Frontend/scripts/fst_to_fsdb.sh <input.fst> [output.fsdb]`
  - 若不传 `output.fsdb`，默认在输入文件同目录下生成同名 `.fsdb`
  - 用于 FST 波形；frontend 默认构建产物是 `.fst`
  - 若显式执行 `make verilog FRONTEND_WAVEFORM_FORMAT=vcd` 或 `make frontend FRONTEND_WAVEFORM_FORMAT=vcd`，则 frontend pylib 会改为生成 `.vcd`
  - 一旦 `build-frontend/.waveform_format` 已记录为 `vcd`，后续不带参数的 `make frontend` 会沿用 `vcd`；只有显式指定 `FRONTEND_WAVEFORM_FORMAT=fst` 才会切回 `.fst`
  - 中间 `.vcd` 放在临时目录，脚本结束后自动清理
- `scripts/gen_coverage_html.sh`
  - 用法: `src/test/python/Frontend/scripts/gen_coverage_html.sh [--ignore-file FILE] [--omit-file FILE] [input.dat ... | input_dir] [output_dir]`
  - 不传输入时，默认收集 `data/*.dat`
  - 单个 `.dat` 默认输出到同目录下的 `<stem>.genhtml/`
  - 多个 `.dat` 或目录输入默认输出到 `coverage.genhtml/`
  - 会自动生成 `merged.info` 并调用 `genhtml --ignore-errors range --filter missing`
  - HTML 行号左侧的 `[ + ]` / `[ - ]` 来自 `merged.info` 里的 `BRDA` 记录，但不一定都是 RTL `if/else` branch。`verilator_coverage -write-info` 会把部分 raw coverage point 转成 lcov `BRDA`；例如端口声明行 `output io_phr_444` 左侧两个 `+`，原始 `.dat` 中对应的是 `t=toggle` 的 `io_phr_444:0->1` 和 `io_phr_444:1->0`。遇到端口、wire、reg 声明行出现 `[ + + ]` 时，应回查 raw `.dat` 的 `t=` 和 `o=` 字段，不要直接解释成代码分支。
  - 若要把指定 `.dat` 合并到已有 `coverage.genhtml/`，可直接执行：
    `source /nfs/share/unitychip/activate && PATH=/nfs/share/unitychip/bin:$PATH src/test/python/Frontend/scripts/gen_coverage_html.sh src/test/python/Frontend/data/runs/<run_id>/coverage`
- `Frontend.ignore`
  - 用于 `toffee_test.reporter.set_line_coverage` 的 line coverage waive。
  - `scripts/gen_coverage_html.sh` 也会默认读取该文件，并把文件级 pattern 转成 `genhtml --exclude`。
  - 当前只 waive 已评审的 MBIST/DFT 文件，保留 SRAM 和 frontend 功能 RTL。
  - 可用 `TB_LINE_COVERAGE_IGNORE=/path/to/file.ignore` 覆盖默认 ignore 文件。
- `Frontend.omit`
  - 用于按源码文本正则 waive 行级覆盖率。
  - `scripts/gen_coverage_html.sh` 默认按每行正则过滤 `lcov` 记录后生成 HTML。
  - pytest teardown 阶段会把每行正则展开成当前 `build-frontend/rtl/*.sv` 的行号范围，再传给 toffee。
  - 当前只 waive 已评审的 MBIST/DFT wiring 行，包括 `io_dft`、`inner_bd`、`inner_childBd`、`childBd`、`boreChildrenBd`、`sigFromSrams`。
  - 可用 `TB_LINE_COVERAGE_OMIT=/path/to/file.omit` 覆盖默认 omit 文件。
  - 可用 `TB_ENABLE_TOFFEE_LINE_COVERAGE=0` 关闭 pytest teardown 阶段的 toffee line coverage 上报。
- `scripts/report_raw_code_coverage.py`
  - 用法: `python src/test/python/Frontend/scripts/report_raw_code_coverage.py --data-dir src/test/python/Frontend/data/runs/<run_id>/coverage`
  - 合并指定同版本 run/suite 的 `.dat`，按 raw 覆盖点输出总 `line/branch/expr/toggle` 覆盖率
  - 使用 `--json-output <path> --run-id <run_id>` 固化机器可读 summary；suite 默认写入 `data/runs/suites/<YYYYMMDD>/<HHMMSS>_<suite_id>/report/code_coverage_summary.json`
  - 同时给出 `ifu_strict`、`ifu_core`、`icache`、`bpu`、`tlb_pmp`、`fault_path` 的 raw line 覆盖率拆分
- `scripts/run_baremode_asm_suite.sh`
  - 每次启动固定写入 `data/runs/suites/<YYYYMMDD>/<HHMMSS>_<suite_id>/`；其中 `cases/<case_stem>/` 是单个用例的独立 run 目录，`report/` 是 suite 汇总。`TB_SUITE_DATE`、`TB_SUITE_TIME` 可用于受控复现，已存在的 suite 目录会直接拒绝，避免覆盖或混写证据。
  - 随后自动生成逐 artifact gate audit、只读反标结果和同签名 `observed` funcov aggregate。
  - `observed` aggregate 只用于批量 summary/unhit；自动 `HIT` 仍只认逐 case、真实 DUT 且通过全部门禁的原始 artifact。
- `scripts/asm_to_jsonl.sh`
  - 用法: `src/test/python/Frontend/scripts/asm_to_jsonl.sh <case.S> [bin_path] [trace_jsonl_path]`
  - 默认把 `.S` 链接到 `0x10001000`，按 NEMU memory base `0x10000000`
    在最终 `.bin` 前补 `0x1000` 字节 0
  - 默认输出 `.bin` 到 `tests/asm_cases/generated/<case>.bin`，输出 golden trace 到
    `data/runs/<run_id>/inputs/<case>.trace.jsonl`
  - 通过现有 `tools/nemu_bin_to_golden_trace.py` 调用 NEMU 并转换 trace；
    raw NEMU log 默认写到 `data/runs/<run_id>/inputs/<case>.nemu.log`
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

- 构建 Verilator 版 Frontend Python DUT：

```bash
make frontend-verilator
```

- 构建 VCS 版 Frontend Python DUT：

```bash
make frontend-vcs \
  FRONTEND_VCS_HOME=/path/to/vcs \
  FRONTEND_VERDI_HOME=/path/to/verdi
```

`make frontend` 默认构建 Verilator 版；`make frontend-vcs` 固定使用 VCS
和 FSDB 波形。两套产物分别保留在
`build-frontend/pylib-verilator/Frontend/` 和
`build-frontend/pylib-vcs/Frontend/`。跑测试时用 `TB_FRONTEND_SIM=verilator`
或 `TB_FRONTEND_SIM=vcs` 选择；只有需要显式指定非默认目录时才使用
`TB_FRONTEND_PYLIB=/path/to/pylib-root`。

- non-DUT 默认回归入口：

```bash
src/test/python/Frontend/scripts/run_pytest_with_log.sh
```

- DUT 集成回归入口：

```bash
TB_ENABLE_DUT_TESTS=1 src/test/python/Frontend/scripts/run_pytest_with_log.sh
```

DUT 批量回归必须看到 pytest final summary 和预期用例数量，不能只看退出码。

- bin trace 列表回归入口：

```bash
src/test/python/Frontend/scripts/run_bin_trace_suite.sh \
  ready-to-run/cfi_mix_case.bin \
  ready-to-run/cfi_random_5inst_case.bin
```

bin 列表必须通过命令行参数或 `--list-file <path>` 显式提供；不要上传默认
active list。长时间运行的大 case 只在需要时手动加入本次命令或本地 list file。

- bin trace 单 case 入口：

```bash
src/test/python/Frontend/scripts/run_bin_trace_pipeline.sh ready-to-run/<case>.bin
```

- 如果需要限定调试窗口，可额外设置 `TB_TRACE_TARGET_CURSOR=<index>`。
  到达该 cursor 只表示该窗口内未发现错误，不是完整 bin-trace 回归通过证据。

- 详细测试参数、bin-trace 环境变量、runtime bound、artifact 规则和
  direct `pytest` 约束，统一见 `docs/agents/frontend-verification.md`。

## 覆盖率闭环

Frontend BT 只保留两条职责不同的覆盖率链：

1. 功能覆盖率：
   `测试点主表 -> 03_funcov_model/frontend_bt_functional_coverage_pilot.csv -> env/funcov/__init__.py + env/funcov/py/icache/__init__.py + env/funcov/py/icache/icache_mainpipe_funcov.py + env/funcov/py/icache/icache_prefetchpipe_funcov.py + env/funcov/py/icache/icache_missunit_funcov.py + env/funcov/py/icache/icache_waylookup_funcov.py + env/funcov/py/icache/icache_hitmiss_funcov.py -> env/functional_coverage.py -> data/runs/<run_id>/funcov/*.funcov.json -> tools/backannotate_funcov.py`。
   registry 当前共 380 行，其中 `Coverpoint` 完整并与 sampler 一一对应的 231 行会装入 runtime；其余 149 行均为 `UNMAPPED` 历史规划。ICache MainPipe、PrefetchPipe、MissUnit、WayLookup、hit/miss 路径分别维护 42、33、31、42、10 个覆盖点于 `env/funcov/py/icache/` 下；IFU 的 CFVec 与 compact/RVC 覆盖点分别维护在 `env/funcov/py/ifu/cfvec_funcov.py` 和 `env/funcov/py/ifu/compact_funcov.py`，FTQ two-fetch 覆盖点按请求、WayLookup、MainPipe、IFU delivery、checker 分组维护，`sampler.py` 仅作为兼容聚合入口。带 `旧BPU_FTQ` 路径的历史行全部保持 `Coverpoint` 为空，不能进入采样或自动反标。
   标准 bin-trace 脚本会自动生成 `TB_RUN_ID` / `TB_ARTIFACT_DIR`；fixture 会将 pytest `funcov_bins` 标记或与汇编 case stem 精确匹配的 registry 行写入 `coverage_targets`。
   `make frontend` 在对应 package 构建成功后生成 `build-frontend/frontend_build_manifest.<sim>.json`。funcov 按 `TB_FRONTEND_SIM` 选择 manifest，且只在其源码状态干净、`simulator` 与已选 package 一致、DUT/RTL/signal-contract 哈希与当前编译产物一致时接受其中的 `dut_source_sha`。
2. 代码覆盖率：
   `dut.SetCoverage() -> Verilator .dat -> toffee set_line_coverage() -> report_raw_code_coverage.py/gen_coverage_html.sh`。

功能覆盖率证明目标场景被激励；checker、assertion、monitor 或 trace 对比证明 DUT 行为正确。代码覆盖率用于发现 RTL 空洞，三者不能互相替代。完整建模、真实 DUT 证据、版本隔离、自动反标和人工 `CLOSED` 规则见
`docs/03_funcov_model/skills.md`。

## 文档分工

- 目录结构、入口分层和当前实现定位，以当前 README 为准。
- frontend 验证流程、bin-trace 运行要求、artifact 规则和提交约束，统一见
  `docs/agents/frontend-verification.md`。
- 测试点驱动的功能覆盖率建模、回归、版本门禁和反标规范，统一见
  `docs/03_funcov_model/skills.md`。
- DUT / monitor / env mismatch 的分析方法，统一见
  `docs/agents/frontend-debugging.md`。
