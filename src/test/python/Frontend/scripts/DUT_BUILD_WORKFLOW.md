# 设计更新后的 Frontend DUT 重编与验收

用户于 2026-10-23 确认此流程为 coord 的默认职责。后续协调和同步仓库时，发现远端 `origin/frontend-bt` 的设计或构建输入更新，即执行完整刷新，不再等待用户逐项提醒。仅文档或 Python 验证代码变化时，先比较依赖，再决定是否需要重编；复用必须保留真实 source 和兼容性证据。此约定不表示已部署后台轮询服务。

## 1. 冻结基线，保护现有工作

1. 检查主仓库、random 仓库及相关 worktree 的状态、未提交修改和现有 build manifest；记录文件哈希。不得覆盖其他 owner 的源码和证据。
2. `git fetch origin frontend-bt` 后解析 `origin/frontend-bt^{commit}`，固定本轮完整 SHA。远端随后新增提交记为下一批，当前构建不追 tip。
3. 比较旧 manifest source 与新 SHA 的设计源、resources、构建脚本、配置及 submodule gitlink。确认设计更新后重编，不以旧 DUT 产生最新结论。仅启动环境或验证侧变化时，记录逐文件等价性证据、实际构建命令和兼容 ledger；复用原 manifest/source，不把二进制冒充为新 SHA 构建。
4. 使用干净的隔离 checkout；新 checkout 和所有临时材料均放在两个长期仓库内部的忽略目录。例如主仓 `build-frontend/build-checkouts/coord-dut-<SHA>-<日期>/`，临时文件使用主仓 `.cache/coord-tmp/` 并显式设置 `TMPDIR`。2026-10-23 的历史构建曾使用系统临时目录，该路径不再作为新任务入口；不要重新创建系统或 workspace 顶层的临时仓库。固定 SHA 应来自本次 fetch 的仓库，不能用另一 clone 中陈旧的 remote ref。
5. 主工作目录有 dirty 修改时，不在其中 pull、reset、clean 或强制切换 submodule。验证分支的同步、提交和推送继续遵守 rebase，禁止 force-push；远端已有的设计 merge 保持原历史。

## 2. 初始化 submodule 与环境

在隔离 checkout 中执行，不运行 `make init-force` 或 `make bump`：

```bash
git submodule sync --recursive
make init
git submodule status --recursive
```

`make init` 除初始化顶层依赖，还初始化 `rocket-chip/{cde,hardfloat}` 和 `XSCache/{OpenNCB,ZhuJiang}`。以父仓库记录的 gitlink 为准，不让 submodule 自行追最新分支。检查所需 submodule 无未初始化、提交不匹配或冲突状态；缺失的其他必需嵌套依赖按固定 gitlink 初始化。可用已有本地镜像加速获取，但必须逐项校验 SHA。

在干净 shell 中启用 Python 环境，并导出绝对源码目录：

```bash
source /nfs/home/zhaoxinran/.venv/mcpgateway/bin/activate
export TMPDIR="/nfs/home/jiabowen/ai_workspace/frontend-bt-jiabowen/.cache/coord-tmp"
mkdir -p "$TMPDIR"
export NOOP_HOME="/nfs/home/jiabowen/ai_workspace/frontend-bt-jiabowen/build-frontend/build-checkouts/coord-dut-${DUT_SHA_SHORT}-${BUILD_DATE}"  # 先设置实际 SHA短码和日期，并创建固定 SHA 的干净 checkout
cd "$NOOP_HOME"
```

不能先 source `/nfs/share/unitychip/activate`：该旧环境可能加载不兼容的 xspcomm。仅在旧环境上叠加 virtualenv 不能证明动态库路径正确。`NOOP_HOME` 必须 export；本轮首次尝试因缺少它，在 RTL 生成后的 difftest 收集阶段失败。

## 3. 并行编译，串行调度

coord 是共享构建和重型 DUT slot 的唯一调度者。开始前记录 owner、任务、源码 SHA 和产物根；不与 ctrl/audit 的重型任务并跑。

```bash
set -o pipefail
make -j32 frontend-verilator \
  CONFIG=DefaultConfig ISSUE=E.b NUM_CORES=1 \
  CHISEL_TARGET=systemverilog FRONTEND_WAVEFORM_FORMAT=fst \
  FRONTEND_BUILD_JOBS=32 2>&1 | tee build-frontend-build.log
```

`-j32` 和 `FRONTEND_BUILD_JOBS=32` 都要设置；实际宿主资源不足时调整并行数并记录。保留完整日志和退出码。任何失败都不能只凭现存 RTL、so 或 manifest 宣布构建成功；先定位失败阶段，隔离不完整产物，再确保生成和编译链完整成功。不得用旧产物补齐后改写 source SHA。

## 4. 构建与接口门禁

由 Makefile 调用 `tools/write_frontend_build_manifest.py` 生成 manifest，不手工替换 source。记录完整 source、implementation、design baseline、submodule SHA、配置、工具版本、generated RTL、Python extension、DUT build 和 signal contract 哈希。

用 `env/runtime/artifact_provenance.py` 的 `load_frontend_build_manifest` 对实际发布目录校验，要求 `build_manifest_status=valid`、实际文件哈希匹配。干净原生构建应有 `source_tree_dirty=false`、`source_sha_override=false`，source 与 implementation 一致。确有适配补丁时记录真实来源、差异和兼容性；manifest 无效时禁止 DUT/HIT 回标。

在同一 virtualenv 和选定构建目录下设置：

```bash
export TB_FRONTEND_SIM=verilator
export PYTHONPATH="$NOOP_HOME/src/test/python/Frontend:$NOOP_HOME/build-frontend/pylib-verilator${PYTHONPATH:+:$PYTHONPATH}"
python -m pytest -q \
  src/test/python/Frontend/tests/py/environment/test_build_manifest_source_scope.py \
  src/test/python/Frontend/tests/py/environment/test_frontend_dut_interface_contract.py
```

同时检查 Python 依赖与 native DUT 导入。以上 interface 测试包含 fake 输入初始化检查，只是接口门禁；必须继续运行真实 DUT。接口位宽、槽数及 packed 格式以新生成的完整信号合同为准，不为迁就旧环境截短信号。

## 5. 真实 DUT smoke 和完整 binary 回放

先在隔离构建中执行短真实 DUT smoke，使用全新 run ID 和独立产物目录：

```bash
TB_ENABLE_DUT_TESTS=1 \
TB_RUN_ID="coord_jal_$(date +%Y%m%d_%H%M%S)_$$" \
python -m pytest -q \
  src/test/python/Frontend/tests/py/zhaoxinran/test_multi_branch.py::test_jal_forward_jump_observes_target_pc
```

通过后将新产物暂存到主仓库的版本目录，使用显式构建路径完成 binary 验收；或像本轮一样在短门禁通过后受控切换默认链接，再验收当前主环境。必须先归档旧默认输出；完整回放失败时不得宣布刷新完成，默认路径按失败原因回退到保留版本或标记不可用。

选择 `ready-to-run/coremark-2-iteration.bin`，通过现有 runner 执行 NEMU → 完整 golden trace → 新 DUT：

```bash
# cd 到实际运行仓库根目录；保持上述干净 virtualenv。
export TB_FRONTEND_SIM=verilator
export TB_NEMU_EXEC="$PWD/ready-to-run/riscv64-nemu-interpreter"
test -x "$TB_NEMU_EXEC"
sha256sum "$TB_NEMU_EXEC" ready-to-run/coremark-2-iteration.bin
export TB_DUT_BUILD_MANIFEST="$PWD/build-frontend/frontend_build_manifest.verilator.json"
export TB_RUN_ID="coord_coremark_$(date +%Y%m%d_%H%M%S)_$$"
export TB_ARTIFACT_DIR="$PWD/build-frontend/artifacts/$TB_RUN_ID"
export TB_NEMU_MAX_INSTR=0 TB_TRACE_LIMIT=0 TB_TRACE_START_INDEX=0
export TB_TRACE_TARGET_CURSOR=0 TB_TRACE_MAX_CYCLES=0
export TB_SKIP_NEMU=0 TB_RUN_DUT=1 TB_PYTEST_TIMEOUT_SECS=6400
set -o pipefail
bash src/test/python/Frontend/scripts/run_bin_trace_pipeline.sh \
  "$PWD/ready-to-run/coremark-2-iteration.bin" \
  2>&1 | tee "build-frontend/${TB_RUN_ID}.pipeline.log"
```

这是每轮新 DUT 的固定验收程序，不是重跑历史随机 seed。每轮重新绑定当前 binary、NEMU、trace 和 DUT 哈希；不能沿用历史 qualification。全程保持串行，运行中继续提供进度。本轮开启默认功能覆盖采样与波形后约需 57 分钟，不据此承诺后续耗时。

通过条件：NEMU `GOOD TRAP` / halt ret=0；pytest 实际执行且通过；`status=completed`、`completed=True`、cursor=全部 trace entries、`target_cursor=0`、完成判定 `pending_work=0`、`monitor_errors=0`；Toffee `run_metadata.checker.status=pass` 且 `error_count=0`。`budget_exhausted`、部分游标、skip、超时、NEMU-only 均不能替代完整回放。`golden_first_mismatch_redirect` 本身是预测恢复记录，需结合 checker 和最终状态判断。

## 6. 发布与交接

稳定产物放主仓库 `build-frontend/versions/<SHA短码>-<日期>/`，包含 RTL、pylib、原始 manifest、配置及门禁证据。默认 `rtl`、`pylib-verilator`、manifest、filelist 和配置标记必须属于同一版本。切换时确保 slot 空闲，不让运行进程读取到混合版本。不能让正式默认路径依赖将来可能删除的 `/tmp` 构建目录。

再次通过 loader 校验，并核对运行前后 binary/NEMU/trace/build/environment 源码哈希。记录 sampler/registry/environment provenance；主环境有本地 WIP 时明确记录其真实哈希。并发文档编辑单独记录并保护，不误写成所有文件完全未变。

状态文件应包括冻结 SHA、负责人、构建及运行目录、命令、门禁、证据哈希、完成或失败状态、slot owner/state。完成后释放 slot，主动向 ctrl/audit 提供同一 manifest 与产物路径。保留旧证据为 historical；此次验收不自动修改 canonical coverage、测试点或 backannotation。

## 已验证基线：2026-10-23

- Remote/source/implementation：`21a770342d329bab3cfd34268bc97250bb708513`。
- Design baseline：`893e1580b705d81f113c30c7a05734e09a08050e`。
- 导入 6 项、manifest/source 单测 12 项、接口 3 项、真实 JAL smoke 1 项通过。
- Coremark：NEMU 正常退出；完整 `663686/663686` 条轨迹，`247570 cycles`；checker PASS、0 errors，monitor 0 errors；pytest 1 passed，3413.64 秒。
- 本轮完成判定 pending_work=0；backend 原始队列计数为 16，两者口径不同，以环境完成判定和 checker 结果为准。
- [当前构建状态](../../../../../build-frontend/coord-latest-build-20261023.json)、[完整回放结果](../../../../../build-frontend/coord-coremark-20261023/RESULT.md)、[原始构建说明](../../../../../build-frontend/versions/21a770342-20261023/BUILD_README.md)。这些是本地 artifact 链接，可能不随 Git 发布。
