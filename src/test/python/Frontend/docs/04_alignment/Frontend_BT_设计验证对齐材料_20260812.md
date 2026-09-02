# Frontend BT 设计-验证对齐材料（2026-08-12）

> 文档状态：本轮已填写草案
> 对齐日期：`2026-08-12`
> 对齐窗口：`2026-07-29` 至 `2026-08-12`
> 统计时间：`2026-08-12 20:06:34 +0800`
> 负责人：`TBD`
> 评审参与人：`设计 / 验证 / TBD`

## 1. 版本基线

设计侧摘要默认只展示设计 baseline 和验证仓库 baseline；DUT 构建、工具、hash、registry/sampler 签名和 evidence root 只作为验证内部追溯信息。

### 1.1 设计版本 baseline

| 项目 | 内容 |
| --- | --- |
| 设计主线 + 合入 commit | `kunminghu-v3 @ 00eb9ee7c544caddef192eef81f9490533ffbad3` |
| 最新 PR 编号 + PR 合入日期 | `#6326 / #6305 / #6324；2026-08-05` |
| 设计 merge commit | `frontend-bt merge fb285779e3207ea165baf104fade00bf15a8d6c1` |

### 1.2 验证仓库 baseline

| 项目 | 内容 |
| --- | --- |
| 验证仓库/分支 + commit | `frontend-bt / frontend-bt @ 101ab48a3f50369833a9acdbe49e622f6adebf00` |
| 验证 commit 日期 | `2026-08-12 16:16:36 +0800` |
| 回归绑定 implementation/DUT commit | `fbaef5906955b224be1542b1b5c49bc8d2ce4016`；该 commit 可从当前 HEAD 到达：`True` |

说明：设计侧 baseline 按本轮已合入 `kunminghu-v3` 的 latest merge parent `00eb9ee...` 填写；当前文档填写 commit 是 `101ab48a...`，DUT manifest 绑定的 implementation/DUT source commit 是 `fbaef590...`。manifest 内部 `design_baseline_sha` 仍为默认值 `fbaef590...`，未单独覆盖成 v3 merge parent；本轮材料按已有 DUT manifest 和 run evidence 填写，不声称当前 HEAD 已重新构建 DUT。

### 1.3 验证内部追溯信息

| 项目 | 内容 |
| --- | --- |
| DUT 构建 manifest | `build-frontend/frontend_build_manifest.verilator.json` |
| manifest 文件签名 | `sha256:0d2c679056b49ea00bd53e26e97cd6377165eb1504fe08c6ffb6524b450bfd70` |
| 仿真工具及版本 | `Verilator 5.048 2026-04-26 rev v5.048` |
| DUT 构建命令/脚本 | `make frontend CONFIG=DefaultConfig ISSUE=E.b NUM_CORES=1 CHISEL_TARGET=systemverilog FRONTEND_WAVEFORM_FORMAT=fst` |
| DUT build hash | `libUTFrontend.so sha256:281bcaa6fdfe8d7ac32afb4395f1ac77c65706c6da27535d856b372995588d2d` |
| DUT Python extension hash | `_UT_Frontend.so sha256:2b4d81c398746ea4be44c4a1ee1cbe97b4a18d3a04b0b8bae1f0a11469ea3c35` |
| generated RTL hash | `build-frontend/rtl/*.sv sha256:3e0a49bd7d897de240620ac3889e8bbbec28f88896941cdc3b78f30c499cb57a` |
| signal-contract hash | `Frontend_offset.yaml sha256:938e135d88a396d60d93ffb90cb578d9a14daeb2df686ca2c1ad70ea9730731c` |
| manifest 生成时间 | `2026-08-07T07:43:33.420633+00:00` |
| source delta 策略 | `source_sha_override=False; policy=none; delta_sha256=e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855; files=[]` |
| 回归时测试点主表版本/签名 | `src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv`；`sha256:bbd0fc69d070d3d568c2444106e7943791b4f9c474d7bccec50d3c6cae02f2ee` |
| 回归时功能覆盖率 registry 版本/签名 | `src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv`；`sha256:c3220b3b38ac06d269e3031803dd983912199824757f8eb1b15a00c254a99ccd` |
| 回归时功能覆盖率 sampler/definitions 签名 | `sampler_sha256:1d2b4209a8ad2d3f8a9097588a3e2a76cfb30cf1bb74518cd1ef8c837b70b49d`；`definitions_sha256:d6cb8024ed1cf63cd8b0806eedfce2ec4d5c998eca8bcc7d18e8e72d30efdb5c` |
| 回归配置与证据根目录 | `asm_all19`；`src/test/python/Frontend/data/runs/suites/20260812/162404_asm_all19` |
| compatibility signature | `218e524db74038be6c830f66c1a458d96f09f1d869b808b3e31b1e24e63d7e5c` |

这些字段不是互相替代关系：commit 标识源码，manifest 标识构建记录，artifact hash 标识实际运行产物，registry/sampler 标识 coverage 语义，run root 标识本次证据集合。任一项变化，都必须新建或重新筛选 evidence，不能与旧结果合并。

## 2. 当前验证状态

### 2.1 测试点与功能覆盖率统计

测试点主表：`src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv`；覆盖率模型：`src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv`；统计时间：`2026-08-14 15:11:47 +0800`。

| 指标 | 数量 | 统计口径/证据 |
| --- | ---: | --- |
| 测试点文档物理行数 | `1672` | `src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv` 文件物理行数 |
| CSV 逻辑记录数 | `1671` | `csv.DictReader` 总记录数 `1671` |
| 叶子测试点数 | `1134` | `report_frontend_verification_alignment.py` leaf denominator |
| 叶子点对应的 mapped coverage 数 | `624` | 按 `report_frontend_verification_alignment.py` 的可执行叶子规则统计；叶子行 `coverage` 字段非空即计入，不要求必须含 `BIN-*` |
| 其中：当前 pilot active BIN 反标叶子数 | `247` | 测试点主表中引用当前 registry active 行的 `BIN-*`；与 pilot active `Coverpoint` 完整行一一对应 |
| 其中：SV ATP 编号反标叶子数 | `107` | 测试点主表中引用已实现 SV functional coverage 的 `ATP-*` 编号；当前不属于 Python pilot registry |
| 其中：SV coverage 无统一 ID 反标叶子数 | `270` | 测试点主表中已有 SV `covergroup/coverpoint/cross` 描述，但未使用 `BIN-*` 或 `ATP-*` 稳定 ID；不能直接计入 Python registry |
| 功能覆盖率 registry 总行数 | `405` | `src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv` 的全部 `Bin_ID` 记录；其中 active `Coverpoint` 完整行 `247`，历史/未激活行 `158` |
| 功能覆盖率 Python sampler active bin 数 | `247` | canonical registry active 行与 `FUNCTIONAL_COVERAGE_SAMPLER_BIN_KEYS` 一致性检查的 active 集合；不是测试点叶子总数 |
| 本轮 Python bin-trace harness 数 | `1` | 本轮 asm suite 使用 `tests/py/zhaoxinran/test_bin_trace_dut.py::test_bin_trace`；不与 directed DUT Python case 重复相加 |
| Directed DUT Python 用例数 | `60` | `TB_ENABLE_DUT_TESTS=1` 门禁保护的 pytest 展开 case，排除 bin-trace、普通 Python 单测、agent 协议单测和信号契约测试；其中当前可运行 `59`，静态 `BLOCKED(environment)` `1` |
| Directed DUT Python 用例文件数 | `6` | 分布于 `tests/py/jiabowen/`、`tests/py/ruierhan/` 和 `tests/py/zhaoxinran/` 的 6 个文件 |
| `.S` 汇编源用例数 | `19` | `src/test/python/Frontend/tests/asm_cases/**/*.S` |
| 已编译 `.bin` 可执行文件数 | `19` | `src/test/python/Frontend/tests/asm_cases/generated/*.bin` |
| 逻辑测试场景数 | `19` | 本轮 suite case 目录去重；不与 `.S`/`.bin` 重复相加 |

| 主状态 | 数量 | 定义 |
| --- | ---: | --- |
| `UNMAPPED` | `510` | 测试点主表当前主状态 |
| `MODELED` | `412` | 测试点主表当前主状态；其中 active pilot BIN `45`、SV ATP `106`、无统一 ID 的 SV coverage `261` |
| `PARTIAL` | `151` | 测试点主表当前主状态 |
| `HIT` | `61` | 测试点主表当前主状态 |
| `CLOSED` | `0` | 测试点主表当前主状态 |
| **合计** | **`1134`** | 等于叶子测试点数 `1134` |

当前主表无 `BLOCKED`、`N-A`、`UNSPECIFIED` 叶子状态；schema error count 为 `0`。

Directed DUT Python 用例按当前 pytest collection 展开如下。该统计只表示当前仓库中可收集的 directed DUT case inventory，不表示本轮 `asm_all19` 已执行这些 case。

| Python 文件 | pytest collect node 数 | 排除的非 DUT node | Directed DUT case 数 | 备注 |
| --- | ---: | ---: | ---: | --- |
| `tests/py/jiabowen/test_functional_coverage_baremode.py` | `11` | `0` | `11` | bare mode、redirect、backpressure、CFI |
| `tests/py/jiabowen/test_icache_mainpipe_miss_response.py` | `11` | `3` | `8` | 排除 1 个 signal contract 和 2 个参数化 agent 协议单测；8 个 DUT case 中 1 个静态 `BLOCKED(environment)` |
| `tests/py/jiabowen/test_two_fetch_directed_flow_dut.py` | `4` | `1` | `3` | 排除 1 个 signal contract |
| `tests/py/ruierhan/test_icache_lowrisk_gap_closure_dut.py` | `5` | `0` | `5` | ICache low-risk gap closure |
| `tests/py/zhaoxinran/test_instr_uncache_port_boundaries.py` | `28` | `1` | `27` | 排除 1 个 signal contract |
| `tests/py/zhaoxinran/test_multi_branch.py` | `6` | `0` | `6` | multi-branch/multi-CFI |
| **合计** | **`65`** | **`5`** | **`60`** | 排除 bin-trace harness；当前可运行 `59`，静态 blocked `1` |

复核说明：若把 `tests/py/zhaoxinran/test_bin_trace_dut.py::test_bin_trace` 的 1 个 harness 计入，上述 DUT 门禁 case 为 `61`；因此“排除 bin-trace 后 61 个”的说法与当前 pytest collection 不一致，排除后应为 `60`。

### 2.2 回归结果

| 维度 | Verilator | VCS/Verdi |
| --- | --- | --- |
| 工具版本 | `Verilator 5.048 2026-04-26 rev v5.048` | `TBD / 本轮未使用 VCS/Verdi evidence` |
| DUT/build manifest | `sha256:0d2c679056b49ea00bd53e26e97cd6377165eb1504fe08c6ffb6524b450bfd70` | `TBD` |
| 回归 run id | `src/test/python/Frontend/data/runs/suites/20260812/162404_asm_all19` | `TBD` |
| 用例总数 | `19` | `0` |
| PASS | `19` | `0` |
| FAIL | `0` | `0` |
| BLOCKED/未执行 | `0` | `TBD` |
| 失败用例及原因 | `NA` | `未执行` |

本轮 Verilator suite 包含 19 个 `.dat` 代码覆盖率输入和 19 个 `.funcov.json` 功能覆盖率 artifact；pytest outcome 为 `passed`，checker status 为 `pass`。

### 2.3 代码覆盖率

代码覆盖率按 `src/test/python/Frontend/scripts/gen_coverage_html.sh` 的 Verilator 路径取得：对 19 个 `.dat` 运行 `verilator_coverage -write-info` 生成 `merged.info`，并按 `Frontend.ignore` 排除 `*/build-frontend/rtl/Mbist*.sv`。当前环境缺少 `genhtml` 命令，因此 HTML index 未生成；本表使用同一 LCOV 输入的 `DA/BRDA` 汇总结果。

| 覆盖类型 | Verilator 分子/分母 | Verilator % | VCS/Verdi 分子/分母 | VCS/Verdi % | 差异/waive/版本说明 |
| --- | ---: | ---: | ---: | ---: | --- |
| Line | `136230 / 230816` | `59.02%` | `TBD` | `TBD` | 来源 `src/test/python/Frontend/data/runs/suites/20260812/162404_asm_all19/report/code_coverage_genhtml/merged.info`；排除 Mbist source files `18` 个 |
| Branch | `386560 / 911590` | `42.41%` | `TBD` | `TBD` | 同上 |
| Toggle | `TBD` | `TBD` | `TBD` | `TBD` | `gen_coverage_html.sh`/LCOV 本轮未提供 toggle 汇总；旧 raw summary 不用于本表 |
| FSM state/transition | `NA` | `NA` | `TBD` | `TBD` | 本轮 Verilator LCOV 未提供 FSM 专项统计 |

LCOV 中间产物签名：`merged.info sha256:1370b47b6138ff128ec1b3d9e263c5168ec2d23217b9c001bc908637e51ca7f4`；汇总文件：`src/test/python/Frontend/data/runs/suites/20260812/162404_asm_all19/report/code_coverage_genhtml/coverage_lcov_summary.json`。

### 2.4 功能覆盖率

| 指标 | 数量 | 说明 |
| --- | ---: | --- |
| Python pilot modeled 分母 | `247` | 当前 registry 中 `Coverpoint` 完整且已接入 Python sampler 的 active bin 数；L0 `14`，L1 `233`。不使用全部 `405` 行作 active modeled 分母 |
| Python pilot 反标覆盖 | `247 / 247` | 测试点主表中 `BIN-*` active bin 映射数 / active registry bin 数；每个 active bin 恰好映射一个叶子 |
| 当前 registry 自动命中 `HIT` 分子 | `TBD` | 新增 `BIN-801..816` 后 registry/sampler 签名已变化，尚未生成与当前 247-bin 定义兼容的 DUT aggregate |
| 上一兼容快照 Python 功能覆盖率 | `33 / 231 = 14.29%` | `asm_all19_observed.funcov.json` 绑定旧 registry `c3220b3b...` 和旧 sampler `1d2b4209...`；不可与当前 247-bin 分母拼成 `33/247` |
| SV ATP coverage 反标 | `107` | 已在 SV functional coverage 代码中实现并回填到测试点主表的 ATP 编号；本轮 Python `.funcov.json` 不覆盖该链路 |
| SV coverage 无统一 ID 反标 | `270` | 已有 SV covergroup/cross 反标描述，但缺稳定 registry ID；需后续建立 SV coverage registry/导出链后才能纳入统一分母 |
| 待人工 review | `61` | 测试点主表当前 `HIT` 状态数，尚未升级为 `CLOSED` |
| 人工确认 `CLOSED` | `0` | 当前主表无人工闭环项 |
| BLOCKED/N-A | `0` | 当前主表无此类主状态 |

本轮命中覆盖组：`ifu_instr_size_type`、`ifu_pc_step_type`、`ifu_boundary_event`、`ifu_fetch_block_position`、`ifu_cfi_decode_type`、`ifu_instr_compact_source`、`two_fetch_ftq_eligibility`、`two_fetch_pointer_advance`、`two_fetch_waylookup_result`、`two_fetch_mainpipe_hit_pattern`、`two_fetch_ifu_window`、`two_fetch_ifu_source`、`two_fetch_cross_block`、`two_fetch_delivery`。

统计口径说明：测试点主表的 `MODELED/PARTIAL/HIT` 是叶子测试点的主状态，不等于 coverage registry bin 数；`coverage` 列是反标文本，既包含当前 Python pilot 的 `BIN-*`，也包含 SV functional coverage 的 `ATP-*` 或未分配稳定 ID 的 `covergroup/coverpoint/cross`。当前 405 行 pilot registry 中只有 247 行为 active Python model，且这 247 个 active `BIN-*` 已全部在测试点主表中反标一次；因此不能用 `412` 与 `405`，或用所有 `coverage` 非空行与 Python pilot 行数直接做一一比较。

本轮增补 `BIN-801..816`，覆盖 cacheable 取指 IFU 入口握手与反压、单/双块窗口、FTQ/地址/预测 meta 从 ICache 返回到 s1 的逐事务一致性，以及 backend/wb/BPU flush 对旧返回的裁剪。新增模型已通过 FakeDut 状态机、registry/sampler 一致性、测试点一对一反标和当前 `Frontend_offset.yaml` 信号契约单测；尚未把这些 bin 标为 `HIT`，需使用当前 registry/sampler 重新运行 DUT 场景后再回填证据。

增补结论：测试点主表中已经存在的 107 个 `ATP-*` 以及 270 条无统一 ID 的 SV coverage 反标，说明 SV coverage 实现与测试点映射已经有实质内容，但它们不是当前 Python recorder 的 canonical registry。直接把这些描述追加到 `frontend_bt_functional_coverage_pilot.csv` 会违反现有 registry/sampler 一致性契约，也会使本轮 Python artifact 的定义签名失真；后续应单独建立 SV coverage registry/导出和命中证据接入，再合并到统一 coverage 报告。

注意：`report_frontend_verification_alignment.py` 当前对本 run 的 aggregate artifact 自动筛选结果为 `eligible_artifact_count=0`，排除原因包含 `provenance_mismatch:source_delta_policy` 和 `provenance_mismatch:source_delta_sha256`。人工核查发现 aggregate provenance 与 manifest 均为 `source_sha_override=false/policy=none/empty delta`，此处更像 report expected-provenance 字段缺失导致的工具口径问题，应在后续修正脚本后再用自动筛选数替代本节人工汇总。

## 3. 过去两周设计变更及验证影响

范围覆盖最近两次 `kunminghu-v3` 合入到 `frontend-bt` 的设计主线增量：

- `2026-07-22` merge `29c99bba49cd8d0d086ee5bebe631e75a1378136` 到 `2026-07-29` merge `01dffc70f089239145fe966d053170b9870ecc80`：设计主线从 `06f4a74041023799b9ed32c0447c7d55c762f999` 到 `c1bf7dc2e5fb98212ff8c3714bf1241bf699f6f2`。
- `2026-07-29` merge `01dffc70f089239145fe966d053170b9870ecc80` 到 `2026-08-05` merge `fb285779e3207ea165baf104fade00bf15a8d6c1`：设计主线从 `c1bf7dc2e5fb98212ff8c3714bf1241bf699f6f2` 到 `00eb9ee7c544caddef192eef81f9490533ffbad3`。

| 日期 | commit / PR | 设计改动精简描述 | 改动类型 | 验证影响分类 | 测试点动作 | 验证环境动作 | 用例动作 | 功能覆盖率动作 | 优先级 | 验证状态 | 证据/责任人 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `2026-07-24` | `0454478c247c7137301276b85173ed5a99f786bf` / `6253` | `refactor(Frontend): replace takenCfiOffset with endPosition (#6253)` | `refactor` | `CFI offset/end-position 语义；影响 BPU/FTQ bundle、FTQ enqueue/redirect 信息、ICache helper/mainpipe/prefetch pipe 的边界位置表达` | `复核 CFI decode、指令起点/终点、cross-block、two-fetch delivery 相关测试点是否从 offset 语义迁移到 endPosition 语义` | `需确认 sampler/monitor 观测字段不再依赖 takenCfiOffset；signal contract hash 必须绑定本次 DUT` | `本轮 asm_all19 已覆盖 CFI decode、mixed RVC/RVI boundary、cross-block、two-fetch delivery；仍需补一条直接检查 endPosition 与 taken CFI 目标边界一致性的 case` | `已通过 IFU boundary/CFI/two-fetch bins 部分覆盖；建议新增 endPosition 专项 bin 或在现有 CFI bins 备注语义迁移` | `高` | `部分完成` | `本轮 33 个 L1 hit bins 覆盖 IFU/CFI/two-fetch 相关路径；缺 endPosition 专项断言/coverage` |
| `2026-08-05` | `4c2f278ceb756fd24512825111a703f1eb717900` / `6326` | `timing(abtb): delay entry write updates (#6326)` | `fix/timing` | `BPU/ABTB entry write 时序路径` | `评估 ABTB write/update 相关测试点是否需要补充分支预测器时序观测项` | `无需重建 harness；需确认观测信号仍稳定` | `需补 ABTB write 延迟场景或确认现有 BPU directed case 覆盖` | `需补/复核 ABTB entry write coverage bin` | `中` | `计划中` | `本轮 asm_all19 主要覆盖 IFU 边界/CFI/two-fetch；未形成 ABTB/BPU 专项闭环` |
| `2026-08-05` | `0f7122f1397e1f30eeabaa64a4438da20129e2c2` / `6305` | `fix(abtb): train counters on not-taken S3 predictions (#6305)` | `fix/timing` | `BPU/ABTB not-taken 训练计数器功能路径` | `需要新增或映射 not-taken S3 prediction training 测试点` | `无需重建 harness；需确认观测信号仍稳定` | `需新增 not-taken 分支训练 directed case` | `需新增/映射 train counter not-taken bins` | `高` | `计划中` | `本轮 asm_all19 主要覆盖 IFU 边界/CFI/two-fetch；未形成 ABTB/BPU 专项闭环` |
| `2026-08-05` | `57563d5b72ef487e00e407e7abe3c4d3f36b5805` / `6324` | `timing(Bpu): optimize s3_takenMask timing (#6324)` | `fix/timing` | `BPU s3_takenMask timing/内部信号路径` | `复核 s3_takenMask 相关观测点是否可见` | `无需重建 harness；需确认观测信号仍稳定` | `视信号可观测性补 BPU timing smoke 或 directed case` | `复核 BPU takenMask coverage 是否已有模型` | `中` | `计划中` | `本轮 asm_all19 主要覆盖 IFU 边界/CFI/two-fetch；未形成 ABTB/BPU 专项闭环` |

本轮已完成的验证动作集中在 IFU 指令边界、CFI decode、two-fetch 和 FTQ delivery；对 `#6253` 的 endPosition 语义迁移已有相关路径覆盖，但缺少直接面向 endPosition 的专项断言/coverage。对 2026-08-05 合入的 ABTB/BPU 变更，目前只完成基础回归不失败的间接检查，尚未完成针对性功能覆盖率闭环。

## 4. 对齐事项与遗留问题

### 4.1 上次遗留问题跟踪

| ID | 上次记录事项 | Owner | 优先级 | 本次状态 | 已采取动作 | 验收证据 | 下一步/截止日期 |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `A-001` | `周度材料需区分设计摘要和验证内部追溯` | `验证` | `高` | `已解决` | `新增模板并在本文件按内部/外部口径分层填写` | `04_alignment/Frontend_BT_设计验证对齐材料模板.md`；本文件 | `评审后固化字段` |

### 4.2 本次新增事项

| ID | 问题/需求 | 提出方 | 接收方 | 类型 | 优先级 | 期望日期 | 当前状态 | 验收标准 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `A-NEW-001` | `genhtml 当前环境不可用，无法生成 HTML index` | `验证` | `验证/环境` | `风险` | `中` | `2026-08-16` | `待确认` | `gen_coverage_html.sh 可完整生成 index.html，且 Line/Branch 与本轮 LCOV 汇总一致` |
| `A-NEW-002` | `alignment report expected provenance 缺少 source_delta_* 字段导致本轮 aggregate artifact 被自动排除` | `验证` | `验证` | `修复` | `高` | `2026-08-16` | `计划中` | `report_frontend_verification_alignment.py 对 source_delta_policy/source_delta_sha256/source_delta_files 与 artifact provenance 一致匹配` |
| `A-NEW-003` | `2026-08-05 ABTB/BPU 变更缺少专项 directed/funcov 闭环` | `验证` | `验证/设计` | `需求` | `高` | `2026-08-23` | `计划中` | `新增或确认 ABTB not-taken training、entry write、s3_takenMask 相关 testpoint/case/coverage，并有真实 DUT evidence` |
| `A-NEW-004` | `部分 baremode 基础 case PASS 但无 target bin ids，不能直接贡献 mapped funcov 分子` | `验证` | `验证` | `风险` | `中` | `2026-08-16` | `待确认` | `明确这些 case 是 smoke 还是需要绑定 registry bin；若绑定，更新 registry/testpoint/evidence` |

## 5. 结论与决策记录

| 项目 | 结论/决策 | Owner | 截止日期 | 评审确认 |
| --- | --- | --- | --- | --- |
| 版本基线是否锁定 | `设计侧 baseline 锁定 kunminghu-v3 00eb9ee...；DUT evidence 锁定 fbaef590...；材料填写环境为 frontend-bt 101ab48...` | `验证` | `2026-08-12` | `待评审` |
| 回归是否可作为当前签核证据 | `可作为 fbaef590... DUT 的 asm_all19 Verilator directed evidence；不能代表当前 HEAD 已重建 DUT` | `验证` | `2026-08-12` | `待评审` |
| coverage 差异是否接受 | `本轮代码覆盖率采用 gen_coverage_html/verilator_coverage LCOV 口径；旧 raw summary 不用于签核表` | `验证` | `2026-08-12` | `待评审` |
| 需新增/修改/删除的验证内容 | `补 ABTB/BPU 变更专项；修正 alignment report provenance；补 genhtml 环境` | `验证` | `2026-08-23` | `待评审` |

## 6. 证据索引

| 证据类型 | 路径/链接 | 生成时间 | 对应版本/签名 | 备注 |
| --- | --- | --- | --- | --- |
| 测试点主表 | `src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv` | `统计于 2026-08-14 15:11:47 +0800` | `sha256:9d0904233dbe504c6be4cb883fcc395d0b9f502347c8569c1793ce0048109ad3` | 叶子测试点 `1134`；mapped `624` |
| coverage registry/model | `src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv` | `统计于 2026-08-14 15:11:47 +0800` | `registry_sha256:e4ff81d3a15fd912d5618ad181524a0e00cb4d65b2716dcb4950a93c985be66d; sampler_sha256:66e5d5fc29cd9332fba062ab2f00c39e65a2ef697f062d7a7345faf8ed8a93da; definitions_sha256:a6d7ac1f5b0e8aaf10a6450324e241f0faa6d53cbd3a3c99c867a54bfd8c0a4c` | registry bins `405`；active `247` |
| Python bin-trace harness | `src/test/python/Frontend/tests/py/zhaoxinran/test_bin_trace_dut.py::test_bin_trace` | `本轮 suite 调用` | `frontend-bt current HEAD` | bin-trace DUT harness `1` 个 |
| Directed DUT Python 用例清单 | `src/test/python/Frontend/tests/py/jiabowen/test_functional_coverage_baremode.py`；`src/test/python/Frontend/tests/py/jiabowen/test_icache_mainpipe_miss_response.py`；`src/test/python/Frontend/tests/py/jiabowen/test_two_fetch_directed_flow_dut.py`；`src/test/python/Frontend/tests/py/ruierhan/test_icache_lowrisk_gap_closure_dut.py`；`src/test/python/Frontend/tests/py/zhaoxinran/test_instr_uncache_port_boundaries.py`；`src/test/python/Frontend/tests/py/zhaoxinran/test_multi_branch.py` | `2026-08-13 pytest --collect-only` | `frontend-bt current HEAD` | 排除 bin-trace 后 DUT 门禁展开 case `60`；可运行 `59`，静态 blocked `1` |
| asm `.S`/`.bin` manifest | `src/test/python/Frontend/tests/asm_cases/jiabowen/`；`src/test/python/Frontend/tests/asm_cases/zhaoxinran/`；`src/test/python/Frontend/tests/asm_cases/generated/` | `本轮 suite 调用` | `.S=19; .bin=19` | 逻辑场景 19 个 |
| Verilator 回归报告 | `src/test/python/Frontend/data/runs/suites/20260812/162404_asm_all19` | `2026-08-12 16:24:04 run id` | `compatibility_signature:218e524db74038be6c830f66c1a458d96f09f1d869b808b3e31b1e24e63d7e5c` | 19/19 PASS |
| VCS/Verdi 回归报告 | `TBD` | `TBD` | `TBD` | 本轮未执行 |
| 功能覆盖率 aggregate（上一兼容快照） | `src/test/python/Frontend/data/runs/suites/20260812/162404_asm_all19/report/funcov/asm_all19_observed.funcov.json` | `本轮 suite 生成` | `registry_sha256:c3220b3b38ac06d269e3031803dd983912199824757f8eb1b15a00c254a99ccd; sampler_sha256:1d2b4209a8ad2d3f8a9097588a3e2a76cfb30cf1bb74518cd1ef8c837b70b49d` | target bins `33`；与当前 247-bin model 签名不兼容 |
| Verilator LCOV merged info | `src/test/python/Frontend/data/runs/suites/20260812/162404_asm_all19/report/code_coverage_genhtml/merged.info` | `2026-08-12 20:06:34 +0800` | `sha256:1370b47b6138ff128ec1b3d9e263c5168ec2d23217b9c001bc908637e51ca7f4` | 由 `verilator_coverage -write-info` 生成 |
| Verilator LCOV 汇总 | `src/test/python/Frontend/data/runs/suites/20260812/162404_asm_all19/report/code_coverage_genhtml/coverage_lcov_summary.json` | `2026-08-12 20:06:34 +0800` | `sha256:74ab91c7fbe75f15d17575ed9f6d7c9b21ab49fa938c4ad24897bd78802e7087` | HTML 未生成：`genhtml` 不在 PATH |

## 附：字段冗余性判断

1. `DUT manifest path` 与 `build_manifest_sha256` 不重复：前者定位文件，后者证明内容未变。
2. `dut_source_sha`、`implementation_sha`、`design_baseline_sha` 不重复：分别表示 DUT 源码、生成 manifest 时的验证实现 HEAD、语义设计基线。当前三者相同，但 source override 场景下可以不同。
3. `dut_build_sha256`、`dut_python_extension_sha256`、`generated_rtl_sha256`、`signal_contract_sha256` 不重复：分别锁定运行库、Python 装载模块、生成 RTL、信号 offset contract。
4. `registry_sha256` 与 `sampler_sha256` 不重复：registry 定义 bin，sampler 定义如何从 DUT 信号采样命中。
5. `run root` 与单个 artifact hash 不重复：run root 说明证据集合范围，artifact hash 证明单个输入/输出内容。
