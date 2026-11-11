# 母 worktree 分支并入 V2 本地分支的冲突复核记录

## 1. 范围与术语

| 术语 | 本文含义 |
|---|---|
| 母分支 | `codex/memblock-csr-prefix-fix-20261025`，本轮源提交为 `5b404c9a1ad1a3d5fad7eb67e7aaccc89498806d`。 |
| 目标分支 | 当前主工作区的 `mem_ut_uvm_v2`，合并前提交为 `153977460cfb46108da0ad186c9b13f561b39f9c`。 |
| 快进合并 | 目标提交是母分支提交的祖先，只前移目标分支引用，不另造合并提交。 |
| 本地待保留改动 | 合并前未提交的 24 个已跟踪文件修改和 17 个未跟踪文件；不属于母分支的 51 个提交。 |
| 严格 / 稀疏物理窗口 | `MEMBLOCK_MAIN_MEM_RANGES_EN=1/0`；前者限制共享内存访问窗口，后者允许按可路由物理地址与 PMA 属性选择有效映射。 |

本轮只合并本机母分支，不执行 `fetch`、`pull`、`rebase` 或合入 `origin/mem_ut_uvm_v2`，也不向远端推送。母 worktree 自身未跟踪的 `mem_ut/ver/ut/memblock/signal.rc` 不在提交历史中，本轮没有带入目标分支。本记录只审查合并拓扑和本地冲突取舍；母分支各功能的实现级正确性仍以其已有 plan、review 与验证记录为准。

## 2. 合并前复核

`git rev-list --left-right --count mem_ut_uvm_v2...codex/memblock-csr-prefix-fix-20261025` 返回 `0 51`，共同祖先即目标分支原 HEAD，因此已提交历史可快进，不存在两个提交分支之间的内容冲突。母分支这 51 个提交涉及测试框架、L2TLB、RM、脚本、配置和配套文档；本轮没有修改母分支 worktree。

为避免覆盖原工作区，先将 41 项未提交内容保存到 `8198b88de62481102471970819013da4264637aa`（`codex-before-mother-to-v2-20261103`），再快进目标分支并恢复该快照。快照暂保留，便于复核合并前的原值；它不是待合入母分支的提交。

## 3. 冲突与取舍

恢复快照时，9 个 testcase preset 在同一个字段发生实际文本冲突：原本地值 `+MEMBLOCK_MAIN_MEM_RANGES_EN=1`，母分支值 `+MEMBLOCK_MAIN_MEM_RANGES_EN=0`。逐个检查冲突块后确认均只有该字段冲突，且两侧均保留 `+MEMBLOCK_PMA_PMP_MODEL_EN=1`。按用户指定的“原则上以母 worktree 为主”，九处全部取母分支的 `0`：

- `tc_dispatch_real_l2cache_model.cfg`
- `tc_dispatch_real_mixed_sta_wb_smoke.cfg`
- `tc_dispatch_real_mixed_wb_smoke.cfg`
- `tc_dispatch_real_multi_store_wb_smoke.cfg`
- `tc_dispatch_real_smoke.cfg`
- `tc_dispatch_real_smoke_flushsb.cfg`
- `tc_dispatch_real_store_smoke.cfg`
- `tc_dispatch_real_store_sta_wb_smoke.cfg`
- `tc_dispatch_real_store_wb_smoke.cfg`

以上文件均位于 `mem_ut/ver/ut/memblock/seq/plus_cfg/`。母分支的 `memblock_l2tlb_routed_ppn_coverage_plan_20261102.md` 对稀疏模式的最终物理地址路由与 PMA 交集有明确设计和验收记录；这里取 `0` 与该方案一致。若改回本地 `1`，这些 smoke 将继续走严格窗口，无法按母分支的默认测试意图覆盖稀疏路由。此决策只解决配置冲突，不声称重新完成该方案的仿真验收。

其余重叠内容按以下方式复核：

| 类别 | 文件或数量 | 处理结果 |
|---|---|---|
| 仅本地修改、母分支未改 | `AGENTS.md`、两份 framework_design 文档、两份 StoreQueue 文档、`lsq_admission_flow.md`，共 6 个 | 恢复后字节内容与合并前快照一致。 |
| 两侧相同 | 6 个 `plus_cfg` preset | 母分支提交已包含原本地值，无额外工作区差异。 |
| 母分支包含原本地开关并新增参数 | `default.cfg` | 保留母分支完整版本，包含原本地的 PMA/PMP 与 dispatch 默认开启，以及新的 PPN 窗口参数。 |
| 两侧独立新增索引 | `AI_DOC/analysis/rtl/v2/index.md` | 同时保留母分支后端写回 flow 入口和本地 CBO flow 入口。 |
| 两侧扩展参数说明 | `AI_DOC/project_management/mem_ut_parameter_management.md` | 同时保留母分支的 routed PPN 参数/窗口语义与本地 `lsqCanAccept` 公共 admission gate 说明。后者描述后续建模条件，与母分支尚在 `undo` 的 LSQ RTL 等价 admission plan 不冲突。 |
| 仅本地未跟踪 | 17 个文档、Word 与配图文件 | 恢复后与快照逐字节一致，仍未跟踪、未暂存。 |

## 4. 结果与验证边界

目标分支现已快进到 `5b404c9a1a`；没有未解决冲突或暂存内容。恢复后的工作区仅保留 8 个已跟踪文档修改、原有 17 个未跟踪文件及本记录；九处 `.cfg` 取母分支值后不再形成工作区差异。原有 CBO 索引入口指向仍未跟踪的 CBO 文档，后续若提交这些本地文档，应把入口与对应文件一同纳入。

本轮已核对：原本地 24 个已跟踪改动的 6/6/1/9/1/1 类取舍；17 个未跟踪文件逐字节保留；`git diff --check`、两个已变更 Shell 脚本的 `bash -n`、两个新增 Python 检查脚本的语法解析均通过。随后按 V2 项目远端入口在 `mem_ut/ver/ut/memblock/sim` 执行 `make eda_compile tc=tc_sanity mode=base_fun` 和 `make eda_run tc=tc_sanity mode=base_fun`，两者退出码均为 0。`tc_sanity` 日志 `base_fun/log/tc=tc_sanity_ts=virtual_base_sequence_cfg=default_seed=666666_rtl.log` 显示 `TEST CASE PASSED`、`UVM_ERROR=0`、`UVM_FATAL=0`；同时有 244 个 `UVM_WARNING`，主要是此 sanity 用例未建立主表时默认 LSQ/issue sequence 的等待提示。该用例不证明新路由策略或多 seed 回归正确，相关专项仍以母分支已有 implementation review 与验证记录为准；basicTest 多 seed 结果见下方。

本地脚本专项测试结果：`test_run_eda_regression_plus_file.sh`、`test_run_function_cfg_cov_matrix.sh`、`test_run_eda_regression_identity_race.sh` 均通过；`test_runtime_parameter_priority.sh` 失败。失败原因可由脚本第 9、24、27 行直接复核：先创建 `$ROOT/cfg` 作为输入配置目录，随后 `dry cfg preset` 又把同一路径作为必须为空的 `--output-dir`，runner 因“output directory is not empty”在测试断言前拒绝执行。这是母分支提交内的独立测试脚本问题，不是本轮合并造成的配置冲突；本轮没有修改该脚本，修复应由用户另行确认范围。

独立合并 review：subagent `mother_merge_review` 于 2026-11-03 完成只读复核，结论为无合并阻塞、可继续 basicTest 多 seed。review 独立确认 51 个提交完整纳入、远端 6 个独有提交未并入、本地 24 tracked/17 untracked 快照均已妥善保留、9 处 sparse 配置与母分支计划一致，未发现索引或参数文档遗漏。Reviewer 指出默认 `basicTest + default.cfg` 使用 `MAIN_MEM_RANGES_EN=1`，因此下面的随机回归不直接覆盖九个取值为 0 的 preset；母分支已有参数优先级测试脚本的目录冲突缺陷也未由本轮修改。

合并 review 后，按 `AI_DOC/project_management/mem_ut_parallel_regression_execution_rule.md` 使用 `run_eda_regression.sh` 执行 3 个随机 seed：`1711560097`、`1153317449`、`1730109137`。参数为 `--tc basicTest --cfg default --mode merge_basic_v2_20261103 --compile --no-wave --no-coverage --jobs 3 --monitor --record --error-limit 1`。结果目录为 `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/.eda_regression/mother_merge_basic_20261103_3seed`；`summary.log` 的 `result=PASS`、`pass=3`、`fail/infra/incomplete/aborted/pending=0`、`remote_unknown_jobs=0`，`results.tsv` 三个 seed 均为 `PASS` 且 `uvm_count=0`。编译由该回归在独立 mode 中执行并成功。此回归覆盖母分支集成后的默认 basicTest 路径，不替代各专用 cfg 的 sparse 路由专项验证。

另对母分支提交区间执行 `git diff --check` 时，在两份已提交 plan/review 文档末尾发现多余空行提示；这不是本轮冲突，也不影响快进。为了保持母分支原提交内容，本轮未擅自改写该两份文档。快照 `8198b88de62481102471970819013da4264637aa` 保留作本地恢复核对，不应直接 `stash pop`，否则会再次尝试应用旧的九处严格窗口值。
